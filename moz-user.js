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

// Don't accidentally quit the whole browser when hitting Ctrl-Q
user_pref("browser.quitShortcut.disabled", true);

// Don't show suggested sites in the New Tab page
user_pref("browser.newtabpage.enhanced", false);

// Search using Google
user_pref("browser.search.defaultenginename", "Google");

// Don't recommend search suggestions
user_pref("browser.search.suggest.enabled", false);

// Accepted languages
user_pref("intl.accept_languages", "en-ca,en-gb,en,en-us,sv,es,fr");

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

// Don't send telemetry or health reports
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);

// Don't allow use of the Beacon APIs
user_pref("beacon.enabled", false);

// Accept cookies, but third-party ones only from sites we've visited
user_pref("network.cookie.cookieBehavior", 3);

// Don't remember form history
user_pref("browser.formfill.enable", false);

// Don't try to guess TLDs for things entered in the address bar
user_pref("browser.fixup.alternate.enabled", false);

// Don't force a wait before installing an addon
user_pref("security.dialog_enable_delay", 0);

// Don't warn about making changes in about:config
user_pref("general.warnOnAboutConfig", false);

// Make scrollbars wider than the default
user_pref("widget.non-native-theme.scrollbar.size.override", 28);

// Don't allow sites to make scrollbars narrow
user_pref("layout.css.scrollbar-width-thin.disabled", true);

// Load userChrome.css at startup
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// Re-enable deprecated "compact mode" UI option
user_pref("browser.compactmode.show", true);
user_pref("browser.uidensity", 1);
