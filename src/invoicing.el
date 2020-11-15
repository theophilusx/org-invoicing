(defvar oi-company '((:name . "Fred Flintstone")
                     (:address1 . "88 Boulder Av.")
                     (:address2 . "Bedrock 2350")
                     (:email . "fred@claytablet.com")
                     (:phone . "5555 555 555"))
      "Contractor company details.")

(defvar oi-invoice-class "traditional-article"
  "The Latex class to use when generating invoices.")

(defvar oi-invoice-style (expand-file-name "~/.spacemacs.d/invoice/invoice")
  "The Latex style file to use when creating invoices.")


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
      (list (cons :name  (org-entry-get top "Name"))
            (cons :address1 (org-entry-get top "Address1"))
            (cons :address2 (org-entry-get top "Address2"))
            (cons :address3 (or (org-entry-get top "Address3") ""))
            (cons :phone (org-entry-get top "Phone"))
            (cons :email (org-entry-get top "Emaiil"))
            (cons :rate (string-to-number (org-entry-get top "Rate")))
            (cons :terms (org-entry-get top "Terms"))))))

(defun oi-insert-invoice-header (ino)
  (insert (format "#+TITLE: %s\n" ino))
  (insert "#+OPTIONS: toc:nil title:nil num:nil\n"
          (format "#+LATEX_CLASS: %s\n" oi-invoice-class)
          "#+LATEX_HEADER: \\usepackage{array}\n"
          "#+LATEX_HEADER: \\usepackage{tabularx}\n"
          (format "#+LATEX_HEADER: \\usepackage{%s}\n\n" oi-invoice-style)))

(defun oi-insert-company-header ()
  (insert "#+EXPORT_LATEX:\n"
          "  \\hfil{\\Huge\\bf Invoice}\\hfil\\bigskip\\break\\hrule\n"
          (format "  %s \\hfill %s \\newline\n"
                  (cdr (assoc :address1 oi-company))
                  (cdr (assoc :name oi-company)))
          (format "  %s \\hfill %s \\newline\n"
                  (cdr (assoc :address2 oi-company))
                  (cdr (assoc :email oi-company)))
          (format "  %s \\hfill %s \\newline\n\\newline\n"
                  (or (cdr (assoc :address3 oi-company)) "")
                  (cdr (assoc :phone oi-company)))
          "#+END_EXPORT:\n\n"))

(defun oi-insert-client-header (client)
  (insert "#+EXPORT_LATEX:\n"
              "  \\begin{tabularx}{\\textwidth}{l X r}\n"
              "    \\begin{tabular}{l}\n"
              "      \\textbf{Invoice to} \\\\\n"
              (format "      \\ %s \\\\\n" (cdr (assoc :name client)))
              (format "      \\ %s \\\\\n" (cdr (assoc :address1 client)))
              (format "      \\ %s \\\\\n" (cdr (assoc :address2 client)))
              (format "      \\ %s \\\\\n" (cdr (assoc :address3 client)))
              "    \\end{tabular}\n"
              "    & \\ &\n"
              "    \\begin{tabular}{|c|c|}\n"
              "      \\hline\n"
              "      \\textbf{Date} & \\textbf{Invoice \\#} \\\\\n"
              "      \\hline\n"
              (format "      \\today & %s \\\\\n" invoice-number)
              "      \\hline\n"
              "      \\textbf{Terms} & \\textbf{Due Date} \\\\\n"
              "      \\hline\n"
              (format "      %1$s & \\AdvanceDate[%1$s]\\today \\\\\n"
                      (cdr (assoc :terms client)))
              "     \\hline\n"
              "    \\end{tabular}\n"
              "  \\end{tabularx}\n"
              "#+END_EXPORT:\n\n"))

(defun oi-create-invoice (invoice-number client period path)
  (let ((inv-file (concat path invoice-number ".org")))
    (save-current-buffer
      (find-file inv-file)
      (beginning-of-buffer)
      (oi-insert-invoice-header invoice-number)
      (oi-insert-company-header)
      (oi-insert-client-header client)
      (save-buffer)
      inv-file)))

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
                                                           period))
          (client-path (file-name-directory (buffer-file-name)))
          (i-file (oi-create-invoice inv client period client-path)))
      (insert (format "   Invoice: %s\n" inv))
      (insert (format "   Period %s to %s\n\n"
                      (plist-get period :tstart)
                      (plist-get period :tend)))
      (insert (format "   ORG: [[file:%s][%s]]\n" i-file inv))
      (insert (format "   PDF: [[file:%s%2$s.pdf][%2$s]]\n\n" client-path inv))
      (org-dynamic-block-insert-dblock "clocktable"))))
