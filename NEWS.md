# shinyslack 0.0.0.9010

* New exported function get_shinyslack_team_id() provides the team id for the Slack workspace used for login.
* Breaking: Removed slack_shiny_ui() from exports.
* Breaking: The site url is now automatically determined, so site_url is no longer an argument to any function.
* Breaking: The "real" ui is no longer automatically wrapped in a cookie handler. Many UIs that use shinyslack won't need to deal with cookies in the normal app, so we shouldn't add extra, unnecessary javascript.
* Abstracted UI switcher into [{scenes}](https://github.com/shinyworks/scenes) package.
* Added a `NEWS.md` file to track changes to the package.
