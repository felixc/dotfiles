import gnomekeyring as gkey


FOLDER_NAMES = (
    ("INBOX", "inbox"),
    ("Sent Items", "sent"),
)


def first(predicate, collection):
    """Return the first item in 'collection' for which 'predicate' is true."""
    return next(x for x in collection if predicate(x))


def local_to_remote(local_name):
    """Convert a local folder name into its remote equivalent"""
    try:
        return first(lambda mapping: mapping[1] == local_name, FOLDER_NAMES)[0]
    except StopIteration:
        return (
            local_name
            if local_name.startswith("lists")
            else local_name.title())


def remote_to_local(remote_name):
    """Convert a remote folder name into its local equivalent."""
    try:
        return first(lambda mapping: mapping[0] == remote_name, FOLDER_NAMES)[1]
    except StopIteration:
        return remote_name.lower()


def get_password():
    """Look up the account's IMAP password from the Gnome Keyring."""
    return gkey.find_items_sync(
        gkey.ITEM_NETWORK_PASSWORD,
        {
            "protocol": "imap",
            "server": "mail.messagingengine.com",
            "user": "felixc@felixcrux.com"
        }
    )[0].secret
