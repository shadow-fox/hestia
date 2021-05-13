;;; init-erc.el --- Summary:

;;; Commentary:

;;; Code:

(defvar +irc-left-padding 13
  "By how much spaces the left hand side of the line should be padded.
Below a value of 12 this may result in uneven alignment between the various
types of messages.")

(defvar +irc-truncate-nick-char ?…
  "Character to displayed when nick > `+irc-left-padding' in length.")

(defvar +irc-scroll-to-bottom-on-commands
  '(self-insert-command yank hilit-yank)
  "If these commands are called pre prompt the buffer will scroll to `point-max'.")

(defvar +irc-disconnect-hook nil
  "Runs each hook when circe noticies the connection has been disconnected.
Useful for scenarios where an instant reconnect will not be successful.")

(defvar +irc-bot-list '("fsbot" "rudybot")
  "Nicks listed have `circe-fool-face' applied and will not be tracked.")

(defvar +irc-time-stamp-format "%H:%M"
  "The format of time stamps.
See `format-time-string' for a full description of available
formatting directives. ")

(defvar +irc-notifications-watch-strings nil
  "A list of strings which can trigger a notification.  You don't need to put
your nick here.
See `circe-notifications-watch-strings'.")

(defvar +irc-defer-notifications nil
  "How long to defer enabling notifications, in seconds (e.g. 5min = 300).
Useful for ZNC users who want to avoid the deluge of notifications during buffer
playback.")

(defvar +irc--defer-timer nil)

(defsubst +irc--pad (left right)
  (format (format "%%%ds | %%s" +irc-left-padding)
          (concat "*** " left) right))


(use-package circe
  :straight t)

(use-package circe-notifications
  :straight t)

(provide 'init-erc)
;;; init-erc.el ends here
