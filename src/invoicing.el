(defvar oi-company '((:name . "Fred Flintstone")
                     (:addr1 . "88 Boulder Av.")
                     (:addr2 . "Bedrock 2350")
                     (:email . "fred@claytablet.com")
                     (:phone . "5555 555 555"))
  "Contractor company details.")


(defun oi-next-invoice-number ()
  "Get the next invoice number, updating the LAST_INV_NO property to the
current value for last invoice number."
  (save-excursion
    (beginning-of-buffer)
    (let* ((entry (search-forward "* Invoices"))
           (prefix (org-entry-get entry "INVOICE_PREFIX"))
           (next-inv (1+ (string-to-number
                          (org-entry-get entry "LAST_INV_NO")))))
      (org-entry-put entry "LAST_INV_NO"
                      (format "%s" next-inv))
      (format "%s%04d" prefix next-inv))))

(defun oi-invoice-period ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let* ((entry (search-forward "* Invoices"))
           (start (org-entry-get entry "PERIOD_START"))
           (end (org-timestamp-format
                 (org-timestamp-from-time (current-time) t)
                 (cdr org-time-stamp-formats))))
      (org-entry-put entry "PERIOD_START" end)
      (message "Start: %s End: %s" start end)
      (list :tstart start :tend end))))

(defun oi-get-client ()
  (save-excursion
    (beginning-of-buffer)
    (let ((top (search-forward "* Client")))
      (list (list :name  (org-entry-get top "TITLE"))
            (list :addr1 (org-entry-get top "Address1"))
            (list :addr2 (org-entry-get top "Address2"))
            (list :addr3 (org-entry-get top "Address3"))
            (list :phone (org-entry-get top "Phone"))
            (list :email (org-entry-get top "Emaiil"))
            (list :rate (string-to-number (org-entry-get top "Rate")))))))

(defun oi-make-new-invoice-header ()
  (interactive)
  (let ((inv (oi-next-invoice-number))
        (client (oi-get-client)))
    (beginning-of-buffer)
    (search-forward "* Invoices")
    (org-insert-heading-after-current)
    (org-demote)
    (insert (format "%s\n\n" inv))
    (let* ((period (oi-invoice-period))
          (org-clock-clocktable-default-properties (append '(:scope file :maxlevel 3)
                                                           period)))
      (message "CLock table props: %s" org-clock-clocktable-default-properties)
      (insert (format "   Invoice: %s\n" inv))
      (insert (format "   Period %s to %s\n\n"
                      (plist-get period :tstart)
                      (plist-get period :tend)))
      (org-dynamic-block-insert-dblock "clocktable"))))
