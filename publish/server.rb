# Static site development server supporting hot reloading.
#
# This is essentially a Ruby EventMachine implementation of the approach in
# https://brandur.org/live-reload.
#
# Usage:
#   bundle exec ruby server.rb
#
# Requirements:
#   sudo gem install bundler
#   bundle install --path ~/.gem
#
# Reference:
# - https://www.engineyard.com/blog/getting-started-with-ruby-and-websockets/
# - Basic EventMachine Sinatra and WebSocket app https://stackoverflow.com/a/43873221
# - EventMachine docs https://www.rubydoc.info/gems/eventmachine/EventMachine/Channel#pop-instance_method

require 'set'
require 'json'
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
        puts "watcher: received: #{data}"
    end

    def unbind
        puts "watcher: Build complete"
        @rebuild_done.push({})
    end
end

# Builder processes source changes from the file system notifications channel,
# executes a build in the background, and notifies the build_completed channel
# on completion.
#
# All actions execute in the EM eventloop by two state machines that coordinate
# via EM queues. The watcher handles batching and deduplication of file system
# notifications. The builder simply builds when signaled and emits a completion
# event.
class Builder
    def initialize(notifications, build_completed)
        @notifications = notifications
        @build_completed = build_completed

        @rebuild_needed = EM::Queue.new
        @rebuild_done = EM::Queue.new
    end

    # Run the state machines. It is an unchecked error to call more once per instance.
    def run
        build_loop
        watcher_idle
    end

    private

    # Idle state: when a notification arrives, trigger a build and transition to building.
    def watcher_idle
        idle = Proc.new do |event|
            puts "q: outer loop triggering build: #{event.absolute_name} (#{event.flags})"
            @rebuild_needed.push(event.absolute_name)
            watcher_building(event.absolute_name)
        end
        @notifications.pop &idle
    end

    # Building state: monitor for build completion while collecting any new
    # notifications. If there are new notifications, trigger a rebuild
    # immediately and remain in this state. If not, transition back to the idle
    # state.
    def watcher_building(last_source)
        backlog = Set.new

        last_rebuild = Time.now
        same_file_quiesce_time = 0.100 # seconds

        collector_sid = @notifications.subscribe do |event|
            # Ignore double modifications for the same file we're already building.
            elapsed = Time.now - last_rebuild
            if event.absolute_name == last_source &&
               elapsed < same_file_quiesce_time
                puts "watcher: detected redundant notification. Skipping: #{event.absolute_name}"
                next
            end

            puts "watcher: detected additional source changed. Enqueueing: #{event.absolute_name} (#{event.flags})"
            @backlog << event.absolute_name
        end

        completion = Proc.new do |event|
            puts "watcher: build completed: #{event}"

            # Notify the application, i.e. websocket clients to reload.
            @build_completed.push({:source => last_source})

            # Unhook the subscriber to prepare to hand off control to the next state.
            @notifications.unsubscribe(collector_sid)

            # No new notifications came in. Return to idle.
            if backlog.empty?
                watcher_idle
                next
            end

            # One or more sources changed during the build--"Doesn't matter
            # who." Trigger a new build and recurse in the building state.
            source = backlog.first
            @rebuild_needed.push(source)
            watcher_building(source)
        end

        @rebuild_done.pop &completion
    end

    # This is the easy part. Build and signal, build and signal...
    def build_loop
        builder = Proc.new do |event|
            puts "builder: building: #{event}"
            EM.popen("make -C .. dev=t", PopenHandler, @rebuild_done)
            @rebuild_needed.pop &builder
        end

        @rebuild_needed.pop &builder
    end
end

# App is a basic static web server that mimics directory indexes as in in the
# deployed CloudFront+S3 site, i.e. / and /notes are rewritten to respective
# index.html. /notes/ is a 404.
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
    EM::WebSocket.run(:host => '0.0.0.0', :port => '5001') do |ws|
        reload_sub = nil

        ws.onopen do |handshake|
            puts "ws: received connection path=#{handshake.path}"

            reload_sub = build_completed.subscribe do |event|
                ws.send({:type => "build_complete", :source => event[:source]}.to_json)
            end
        end

        ws.onclose do |status|
            puts "ws: closed: code=#{status[:code]} reason=#{status[:reason]}"
            build_completed.unsubscribe(reload_sub)
        end

        ws.onerror do |error|
            puts "ws: error: #{error}"
        end
    end
end

# Returns true if a source change should trigger a rebuild. This filters out certain
# emacs save/orgmode publish noise.
def should_build(event)
    # Ignore emacs temp files: .#-emacsasdf, #post.org#
    return false if event.name =~ /^[.]?#/

    # Ignore generated sitemap and feed files. This prevents infinitely looping builds.
    return false if event.name == "index.org"
    return false if event.name == "index.xml"

    return true
end

# Reads notification events when signaled that the notifier handle is readable.
class NotifyHandler < EM::Connection
    def initialize(notifier)
        @notifier = notifier
    end

    def notify_readable
        @notifier.process
    end
end

# Sets up the notifier and connects it to the notify_events channel.
def run_notifier(notify_events)
    notifier = INotify::Notifier.new
    notifier.watch("../notes", :modify) do |event|
        puts "#{event.name} was modified"
        notify_events.push(event) if should_build(event)
    end
    notifier
end

puts "Entering the event loop"
EM.run do
    notify_events = EM::Channel.new
    build_completed = EM::Channel.new

    # 1. File system monitoring
    notifier = run_notifier(notify_events)
    EM.watch(notifier.to_io, NotifyHandler, notifier) do |c|
        c.notify_readable = true
    end

    # 2. Build loop
    Builder.new(notify_events, build_completed).run

    # 3. Websocket server
    websocket_run(build_completed)

    # 4. Development web server
    Thin::Server.start(App, 5000)

    trap('INT') { puts "Shutting down"; EM.stop }
end
