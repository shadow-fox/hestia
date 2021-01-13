;;; init-calendar.el --- Calendar

;;; Commentary:

;;; Code:

(use-package calendar
  :config
  (setq calendar-mark-diary-entries-flag nil)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (concat " (" time-zone ")"))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-christian-all-holidays-flag nil)
  (setq calendar-holidays
        (append holiday-local-holidays  ; TODO set local holidays
                holiday-solar-holidays))

  (use-package solar
    :config
    (setq calendar-latitude 35.17
          calendar-longitude 33.36))

  (use-package lunar
    :config
    (setq lunar-phase-names
          '("New Moon"
            "First Quarter Moon"
            "Full Moon"
            "Last Quarter Moon")))

  :hook (calendar-today-visible-hook . calendar-mark-today))


(provide 'init-calendar)
;;; init-calendar.el ends here
