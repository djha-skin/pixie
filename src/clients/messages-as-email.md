# Proposal for Adding Messaging Platforms as Backends to Himalaya

We should add backends for the following platforms to Himalaya:

- Slack
- Matrix

These are modern messaging platforms that our users likely heavily interact with
everyday. While they are not email providers, we can make them *look* like it
in a practical, intelligent way. This would open the door for all sorts of
advantages:

- Index your slack messages with notmuch
- Have a single "inbox" source for all messages
- No need for large electron heavy clients on machines anymore

These backends can easily be made to fit inside the email model in a
frictionless manner.

Here is a list of features that messaging platforms support and how these
features could easily be ported to an email backend:

- Chat rooms and direct messages can simply be modeled as email destinations
  located in the `To:` line.

- Reply-in-thread on the messaging platforms can be modeled in email using the
  `In-Reply-To` header.

- Quoting a message can be modeled as a forwarded message.

- Chat rooms could be modeled as both IMAP folders and senders. For
  example, if I received a message from the #general channel, I would have
  `general@<slack-domain.com>` as the sender and, when I checked my messages, I
  would have a new email in the `general` folder. Under this scheme, IMAP
  folders would be read-only views; moving emails from one folder to another
  would be dissallowed. Sent messages would appear in the folders
  associated with the channel instead of the "Sent" folder.

- Messages between two people would show up in the "INBOX" folder. That is,
  INBOX would be where DM's reside.

- Messages mentioning the recipient could be presented to the email client as
  "Flagged Important".

- Messages from muted channels could automatically be marked as read.
