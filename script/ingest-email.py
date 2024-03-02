# "Checking the email!"
#
# Pulls unread messages via IMAP, converts each to a post, and pushes each to a
# separate branch and PR. Multiple emails/branches are processed, but only the
# last one will be deployed automatically.
#
# Outputs (side effects):
# - Previously unread messages are marked as read in the IMAP folder
# - One git branch pushed and one PR created per message processed
# - The last new branch is left checked out in the local repo
#
# Requirements:
# - pip install -r requirements.txt
# - pandoc on the PATH
# - Environment variables: IMAP_USERNAME, IMAP_PASSWORD, IMAP_FOLDER,
#   ALLOWED_SENDERS (comma-separated)
#
# To test locally:
# 1. Go to Cloudflare, Email Routing, Email Workers, and disable the route.
# 2. Send a test email.
# 3. source .env_secrets; unset GITHUB_TOKEN
# 4. python script/ingest-email.py


from bs4 import BeautifulSoup
import dkim
import email
import imaplib
import os
import pathlib
import pendulum
import re
import shlex
import subprocess

# Email configuration
imap_server = "imap.gmail.com"
imap_username = os.environ["IMAP_USERNAME"]
imap_password = os.environ["IMAP_PASSWORD"]
imap_folder = os.environ["IMAP_FOLDER"]
allowed_senders = os.environ["ALLOWED_SENDERS"].split(",")

# Blog configuration
notes_dest = "content/notes"
img_dest = "content/assets/images/notes"

#
# Support functions
#

# Converts a post title to a slug suitable for filenames and URL paths
# e.g. "Hello World!" -> hello-world
def slug(title):
    return re.sub(r'[^a-zA-Z0-9]+', '-', title).strip('-').lower()

def summarize_part(part, message):
    content_id = part.get("Content-ID") or "N/A"
    content_type = part.get_content_type()
    content_disposition = str(part.get("Content-Disposition"))
    filename = part.get_filename() or "N/A"
    print(message)
    print(f"  Content-ID: {content_id}")
    print(f"  Content-Type: {content_type}")
    print(f"  Content-Disposition: {content_disposition}")
    print(f"  Filename: {filename}")

#
# Email validation
#

def arc_authenticated(msg_bytes):
    result, dicts, reason = dkim.arc_verify(msg_bytes)
    return result == dkim.CV_Pass

#
# Email to post conversion
#

# Extracts content from a message body. If the message is multipart and has an
# HTML part, return that part converted to orgmode with pandoc. Otherwise return
# the plain text part. Returns None for a multipart message with no text.
def extract_content(msg):
    if not msg.is_multipart():
        return msg.get_payload()

    content = None
    for part in msg.walk():
        if part.get_payload() is None:
            continue

        # Skip attachments.
        content_disposition = str(part.get("Content-Disposition"))
        if "attachment" in content_disposition:
            summarize_part(part, "Skipping attachment")
            continue

        # Skip any part without a payload, e.g. Content-Type: multipart/alternative
        payload = part.get_payload(decode=True)
        if payload is None:
            summarize_part(part, 'Skipping part with no payload')
            continue

        content_type = part.get_content_type()

        # Prefer the HTML part, if one exists. Fall back on plain text by default only.
        if content_type == "text/html":
            # Remove pesky line breaks. Org publish to HTML wraps every
            # paragraph in a <p> element, so we don't need Gmail's BR's at all.
            html = BeautifulSoup(payload.decode(), 'html.parser')
            for br in html.find_all('br'):
                br.extract()
            content = subprocess.run(
                ["pandoc", "--from=html", "--to=org"],
                input=str(html),
                stdout=subprocess.PIPE,
                text=True,
            ).stdout
        elif content_type == "text/plain" and not content:
            content = payload.decode()
        else:
            summarize_part(part, "Skipping non-text part")

    return content

# Scans a multipart message for images. Writes each to a local file. Returns a
# list of metadata maps.
def extract_images(msg, image_dir):
    images = []
    for part in msg.walk():
        if part.get_content_maintype() != "image":
            continue

        filename = part.get_filename()
        if not filename:
            summarize_part(part, "Skipping image with no filename")
            continue

        os.makedirs(image_dir, exist_ok=True)
        image_path = os.path.join(image_dir, filename)
        with open(image_path, "wb") as f:
            f.write(part.get_payload(decode=True))

        images.append({'content_id': part.get("Content-ID").strip("<>"),
                       'filename': filename})

    return images

# Extracts post content and images to local files.
def extract_post(msg, org_dest, img_dest):
    sender = msg["From"]
    subject = msg["subject"]

    # Scream if a message shows up from an unexpected sender.
    if sender not in allowed_senders:
        raise ValueError(f"Unauthorized sender: {sender} ({subject})")

    print(f"Processing email: {subject}")

    files_added = []

    content = extract_content(msg)

    image_path = pathlib.Path(img_dest).joinpath(slug(subject))
    images = extract_images(msg, str(image_path))

    # Rewrite image links from email Content-ID refs to files.
    # What we want is the next line, but it requires Python 3.12 and I can't
    # get pendulum to install in 3.12 in GitHub.
    #image_link_base = image_path.relative_to(org_dest, walk_up=True)
    image_link_base = pathlib.Path('..').joinpath(image_path.relative_to(image_path.parts[0]))
    for img in images:
        org_link = f"file:{image_link_base}/{img['filename']}"
        content = content.replace(f"cid:{img['content_id']}", org_link)

    # Create post file
    note_file_path = os.path.join(org_dest, f"{slug(subject)}.org")
    with open(note_file_path, "w") as f:
        post_timestamp = pendulum.now().format('YYYY-MM-DD ddd HH:mm')
        f.write(f"""#+title: {subject}
#+date: <{post_timestamp}>
#+setupfile: org.txt

{content}""")

    return {'subject': subject,
            'note': note_file_path,
            'images': [i['filename'] for i in images]}


#
# GitHub integration
#

# Executes a system command and throws if it fails.
def cmd(cmd):
    print('[system]', cmd)
    status = os.system(f"{cmd}")
    if status != 0:
        raise Exception(f"Command failed: {status}")

# Adds all post files to git and pushes to a branch.
def check_in(subject, notes_dest, img_dest):
    commit_message = f"Post: {subject}"
    basename = slug(subject)
    branch = f"email/{basename}"
    cmd(f"git checkout -b {branch} origin/main")
    cmd(f"git add {notes_dest}")
    if post['images']:
        cmd(f"git add {img_dest}")
    cmd(f'git commit -m {shlex.quote(commit_message)}')
    cmd(f"git push origin {branch}")
    cmd(f"""gh pr create --assignee shoover --base main \
      --title {shlex.quote(commit_message)} \
      --body '- https://github.com/shoover/shawnhoover.dev/blob/{branch}/content/notes/{basename}.org
- https://stage-www.shawnhoover.dev/notes/{basename}.html'""")


#
# Main
#

imap = imaplib.IMAP4_SSL(imap_server)
imap.login(imap_username, imap_password)
imap.select(imap_folder)

status, email_ids = imap.search(None, "UNSEEN")
email_id_list = email_ids[0].split()
for email_id in email_id_list:
    # Fetch with PEEK to keep the message unread pending successful processing.
    status, email_data = imap.fetch(email_id, "(BODY.PEEK[])")
    if status != "OK":
        raise Exception(f"Fetch {email_id} failed: {status}.")

    msg_bytes = email_data[0][1]

    # Scream if a message shows up failing ARC authentication.
    if not arc_authenticated(msg_bytes):
        raise Exception(f": {sender} ({subject})")

    post = extract_post(email.message_from_bytes(msg_bytes), org_dest=notes_dest, img_dest=img_dest)

    check_in(post['subject'], notes_dest, img_dest)

    imap.store(email_id, "+FLAGS", "\Seen")

imap.logout()

# Leave the last post branch checked out, i.e. readable with `git rev-parse --abbrev-ref HEAD`
