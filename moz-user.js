// Blank home page
user_pref("browser.startup.homepage", "about:newtab");

// Re-open tabs when launching the browser
user_pref("browser.startup.page", 3);

// Always ask where to download files to
user_pref("browser.download.useDownloadDir", false);

// Open new windows in new tabs
user_pref("browser.link.open_newwindow", 3);

// Don't warn when opening multiple tabs
user_pref("browser.tabs.warnOnOpen", false);

// Don't load tabs until switching to them
user_pref("browser.sessionstore.restore_on_demand", true);

// Don't switch to new tabs upon opening them
user_pref("browser.tabs.loadInBackground", true);

// Search using Google
user_pref("browser.search.defaultenginename", "Google");

// Accepted languages
user_pref("intl.accept_languages", "en-ca,en-gb,en-us,en,sv,es,fr");

// Reset print-to-file filename to sensible default
user_pref("print.print_to_filename", "~/mozilla.pdf");

// Don't print anything in the margins
user_pref("print.print_headerleft", "");
user_pref("print.print_headercenter", "");
user_pref("print.print_headerright", "");
user_pref("print.print_footerleft", "");
user_pref("print.print_footercenter", "");
user_pref("print.print_footerright", "");

// Send Do Not Track header
user_pref("privacy.donottrackheader.enabled", true);

// Use Tracking Protection mode everywhere, not just in Private Browsing
user_pref("privacy.trackingprotection.enabled", true);

// Don't remember form history
user_pref("browser.formfill.enable", false);

// Accept cookies, but third-party ones only from sites we've visited
user_pref("network.cookie.cookieBehavior", 3);

// Don't force a wait before installing an addon
user_pref("security.dialog_enable_delay", 0);

// Don't warn about making changes in about:config
user_pref("general.warnOnAboutConfig", false);
