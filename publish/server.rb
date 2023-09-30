# A static site build server for writing notes in orgmode with hot reloading in
# the browser.
#
# When any .org files in notes/ are modified, the site is built in writing mode,
# which includes a little JS in the pages that sets up a WebSocket to receive
# reload messages.
#
# This server answers the question: What if we replaced "python3 -m http.server
# --directory=build 5000" and repeated invocations of "make" and F5 with a
# couple hundred lines of evented Ruby? The design borrows liberally from
# https://brandur.org/live-reload, using channels and an event loop to
# coordinate file system notifications, web sockets, and the build.
#
# Usage:
#   bundle exec ruby publish/server.rb
#   open http://127.0.0.1:5000/notes
#   emacs notes/xyz.org
#   C-x C-s
#
# Requirements:
#   sudo gem install bundler
#   bundle install --path ~/.gem
#
# Reference:
# - Reference EventMachine Sinatra+WebSocket app: https://stackoverflow.com/a/43873221
# - EventMachine docs: https://www.rubydoc.info/gems/eventmachine/EventMachine
# - rb-inotify docs: https://www.rubydoc.info/gems/rb-inotify/0.10.1/INotify/Notifier#watch-instance_method

require 'json'
require 'set'

require 'rb-inotify'
require 'eventmachine'
require 'em-websocket'
require 'rack'
require 'rack-rewrite'
require 'thin'

# An EM popen handler that signals a channel when the watched process exits. All
# process output is printed to the console.
class PopenHandler < EM::Connection
    def initialize(rebuild_done)
        @rebuild_done = rebuild_done
    end

    def receive_data(data)
        puts "watcher: received: <<<#{data}>>>"
    end

    def unbind
        # In theory `get_status` includes the pid and exit code; it seems to be
        # always zeroes.
        puts "watcher: build completed: #{get_status}"
        @rebuild_done.push(get_status)
    end
end

# Processes source changes from the fs notifications channel, executes a build
# in the background, and notifies the build_completed channel on completion.
#
# A watcher state machine receives, batches, and deduplicates fs notifications.
# The build loop simply builds when signaled and emits a completion event. All
# coordination is through EM queues and channels.
class Builder
    def initialize(notifications, build_completed)
        @notifications = notifications
        @build_completed = build_completed

        @rebuild_needed = EM::Queue.new
        @rebuild_done = EM::Queue.new
    end

    # Start the builder. It is invalid (unchecked) to call this more than once.
    def run
        build_loop
        watcher_idle
    end

    private

    # Idle state: upon notification, trigger a build and transition to building.
    def watcher_idle
        idle = Proc.new do |event|
            puts "watcher: idle -> building: #{event.absolute_name} (#{event.flags})"
            backlog = Set.new([event.absolute_name])
            @rebuild_needed.push(backlog)
            watcher_building(backlog)
        end
        @notifications.pop &idle
    end

    # Building state: collect new notifications while monitoring for build
    # completion. If there are new notifications, trigger a rebuild immediately
    # and remain in this state. Otherwise, transition back to idle.
    def watcher_building(last_sources)
        backlog = Set.new

        last_rebuild = Time.now
        same_file_quiesce_time = 0.100 # seconds

        collector_sid = @notifications.subscribe do |event|
            # Ignore double modifications for the same file we're already building.
            elapsed = Time.now - last_rebuild
            if last_sources.member?(event.absolute_name) &&
               elapsed < same_file_quiesce_time
                puts "watcher: detected redundant notification. Skipping: #{event.absolute_name}"
                next
            end

            puts "watcher: detected additional source change: #{event.absolute_name} (#{event.flags})"
            backlog << event.absolute_name
        end

        completion = Proc.new do |status|
            puts "watcher: build completed: #{status}"

            @build_completed.push({:sources => last_sources, :status => status})

            # Unhook the subscriber in preparation to hand off control to the next state.
            @notifications.unsubscribe(collector_sid)

            # No new notifications came in. Return to idle.
            if backlog.empty?
                puts "watcher: building -> idle"
                watcher_idle
                next
            end

            # One or more sources changed during the build--"Doesn't matter
            # who." Trigger a new build and recurse in the building state.
            puts "watcher: building -> building: #{backlog}"
            @rebuild_needed.push(backlog)
            watcher_building(backlog)
        end

        @rebuild_done.pop &completion
    end

    # This is the easy part. Wait, build, signal, repeat.
    def build_loop
        builder = Proc.new do |event|
            puts "builder: building: #{event}"
            EM.popen("make -C .. writing=t", PopenHandler, @rebuild_done)
            @rebuild_needed.pop &builder
        end

        @rebuild_needed.pop &builder
    end
end

# A static web server that mimics directory indexes as in the deployed site,
# i.e. / and /notes are rewritten to respective index.html files. /notes/ is 404.
App = Rack::Builder.new do
    use Rack::Rewrite do
        rewrite '/', '/index.html'
        rewrite '/notes', '/notes/index.html'
    end
    run Rack::Directory.new(File.join(Dir.pwd, '..', 'build'))
end

# websocket_run is an EM websocket server that monitors build completion and
# notifies all clients to reload.
def websocket_run(build_completed)
    EM::WebSocket.run(:host => '127.0.0.1', :port => '5001') do |ws|
        reload_sid = nil

        ws.onopen do |handshake|
            puts "ws: received connection path=#{handshake.path}"
            reload_sid = build_completed.subscribe do |info|
                if info[:status].success?
                    ws.send({:type => "build_complete"}.to_json)
                else
                    puts "ws: build failed #{info[:status]}; skipping reloading"
                end
            end
        end

        ws.onclose do |status|
            puts "ws: closed: code=#{status[:code]} reason=#{status[:reason]}"
            build_completed.unsubscribe(reload_sid)
        end

        ws.onerror do |error|
            puts "ws: error: #{error}"
        end
    end
end

# Returns true if a source change should trigger a rebuild. This filters out
# obvious noise from emacs save-file/orgmode publish.
def should_build(event)
    # Ignore emacs temp files: .#-emacsasdf, #post.org#
    return false if event.name =~ /^[.]?#/

    # Ignore generated sitemap and feed files. This prevents infinitely looping builds.
    return false if event.name == "index.org"
    return false if event.name == "index.xml"

    return true
end

# When signaled that the notifier handle is readable, process the notifications..
class NotifyHandler < EM::Connection
    def initialize(notifier)
        @notifier = notifier
    end

    def notify_readable
        @notifier.process
    end
end

# Sets up a notifier to watch and send notifications to the builder.
def run_notifier(notify_events)
    notifier = INotify::Notifier.new

    # Primarily watch .org sources to rebuild .html
    notifier.watch("../notes", :create, :modify, :delete, :recursive) do |event|
        puts "#{event.name} was modified"
        notify_events.push(event) if should_build(event)
    end

    # If the publish script changes, we want to rebuild the whole site
    notifier.watch("publish.el", :modify) do |event|
        puts "#{event.name} was modified: #{event.inspect}"
        notify_events.push(event) if should_build(event)
    end

    notifier
end

puts "Entering the event loop"

EM.run do
    # Communication channels
    notify_events = EM::Channel.new
    build_completed = EM::Channel.new

    # IO components:
    # 1. File system monitoring
    notifier = run_notifier(notify_events)
    EM.watch(notifier.to_io, NotifyHandler, notifier) do |c|
        c.notify_readable = true
    end

    # 2. Build loop
    Builder.new(notify_events, build_completed).run

    # 3. Websocket server
    websocket_run(build_completed)

    # 4. Web server
    Thin::Server.start(App, 5000)

    trap('INT') { puts "Shutting down"; EM.stop }
end
