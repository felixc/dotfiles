import gnomekeyring as gkey
import itertools


f_names = (
  ('INBOX', 'inbox'),
  ('INBOX.Sent Items', 'sent'),
)


def local_to_remote(name):
  '''Convert a local folder name into its remote equivalent'''
  try:
    return itertools.dropwhile(lambda pair: pair[1] != name, f_names).next()[0]
  except:
    return "INBOX." + name.title()


def remote_to_local(name):
  '''Convert a remote folder name into its local equivalent.'''
  try:
    return itertools.dropwhile(lambda pair: pair[0] != name, f_names).next()[1]
  except:
    return name.replace("INBOX.", "").lower()


def get_password():
  '''Look up the account's IMAP password from the Gnome Keyring.'''
  return gkey.find_items_sync(
    gkey.ITEM_NETWORK_PASSWORD,
    {'protocol': 'imap',
     'server': 'mail.messagingengine.com',
     'user': 'felixc@fastmail.fm'}
    )[0].secret
