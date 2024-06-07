# Introduction

This is my personal `.emacs.d` configuration. To install:

1. Install `emacs-plus` by

   ```sh
   brew install emacs-plus --with-ctags --with-dbus --with-debug \
     --with-mailutils --with-no-frame-refocus --with-xwidgets \
     --with-imagemagick --with-poll --with-modern-black-variant-icon
   ```

2. Clone the repository to home folder (`~`) by

   ```sh
   git clone https://github.com/vcdim/.emacs.d.git ~/.emacs.d/
   ```

3. Ensure there is an `emacs_work.plist` file in `~/Library/LaunchAgents/` folder with the following content:

   ```xml
   <plist version="1.0">
     <dict>
     <key>Label</key>
       <string>emacs_work</string>
       <key>ProgramArguments</key>
       <array>
         <string>/opt/homebrew/bin/emacs</string>
         <string>--fg-daemon=work</string>
       </array>
       <key>RunAtLoad</key>
       <true/>
       <key>KeepAlive</key>
       <true/>
       <key>StandardOutPath</key>
       <string>/tmp/emacs_work.stdout.log</string>
       <key>StandardErrorPath</key>
       <string>/tmp/emacs_work.stderr.log</string>
     </dict>
   </plist>
   ```

4. Launch the server by

   ```sh
   launchctl load -w ~/Library/LaunchAgents/emacs_work.plist
   ```

   This will take some time to get the server ready.

   **TODO**: Dependency hasn't been tested out yet.

5. Make an Automator app (e.g. `Emacs.app`) with the following code

   ```sh
   /opt/homebrew/opt/emacs-plus@29/bin/emacsclient 
     -c -n -q -u \
     -e "(select-frame-set-input-focus (selected-frame))" \
     -s work
   ```

   Also recommend to create a `Capture.app` with the following code

   ```sh
   /opt/homebrew/opt/emacs-plus@29/bin/emacsclient \
     -c -n \
     -F '(quote (name . "capture"))' \
     -e "(select-frame-set-input-focus (selected-frame))" "(my/org-capture)" \
     -s work
   ```

6. Run `Emacs.app` can start a emacs client. Run `Capture.app` can start a capture session.
