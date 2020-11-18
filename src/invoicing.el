(require 'org)

(defgroup org-invoicing nil
  "Customization group for org-invoicing package."
  :version "27.1"
  :tag "Org Invoice Generator"
  :group 'org)

(defcustom oi-company '(:name "My Company Name"
                        :address1 "Address line 1"
                        :address2 "Address line 2"
                        :address3 ""
                        :email "me@company.com"
                        :phone "5555 555 555")
  "A property list describing your company or consulting contact details."
  :type '(plist :key-type (symbol :tag "Attribute")
                :value-type (string :tag "Value"))
  :options `(:name :address1 :address2 :address3 :email :phone)
  :group 'org-invoicing)

(defcustom oi-invoice-class "article"
  "The Latex document class to use for generating PDF invoices."
  :type 'string
  :group 'org-invoicing)

(defcustom oi-invoice-style (expand-file-name"~/.spacemacs.d/invoice/invoice")
  "Location of the invoice.sty Latex style file."
  :type 'file
  :group 'org-invoicing)

(defvar oi-state '(:rate 0.00 :tax-rate 0.00 :tax-name "GST"
                         :expense-amount 0.00 :amount 0.00)
  "Variable which holds current state of invoice generator.")

(defun string->keyword (s)
  "Turn the string 's' into a keyword.
The string will have spaces replaced with '_' and all will be all lower case."
  (intern (format ":%s" (substitute ?_ ?  (downcase s)))))

(defun oi-table->list (table-name)
  "Return a list containing lists representing each row in a table.
The string elements are the cells for each row. The `table-name' argument
specifies a unique table name to identify the table source."
  (save-excursion
    (beginning-of-buffer)
    (search-forward (format "#+NAME: %s" table-name))
    (forward-line 2)
    (seq-filter (lambda (elt)
                  (listp elt))
                (org-table-to-lisp))))

(defun oi-table->plist (table-name)
  "Convert a 2-column name/value mapping table to a plist.
The `table-name' argument is a string specifying a unique table name within the
org client file. Returns a plist where keys are the values from the first column
converted into keywords. All values are strings."
  (let ((data (oi-table->list table-name))
        (data-plist (list)))
    (pcase-dolist (`(,name ,value) data)
      (setq data-plist (plist-put data-plist (string->keyword name) value)))
    data-plist))

(defun oi-next-invoice-number ()
  "Get the next invoice number, updating the 'LAST_INV_NO' property to the
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
  "Retrieves the invoice period start date from the PERIOD_START property within
the 'Invoices' entry. Updates the property to the current date and returns a
plist with keys :tstart and :tend representing the invoicing period."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let* ((entry (search-forward "* Invoices"))
           (start (org-entry-get entry "PERIOD_START"))
           (end (org-timestamp-format
                 (org-timestamp-from-time (current-time) t)
                 (cdr org-time-stamp-formats))))
      (org-entry-put entry "PERIOD_START" end)
      (list :tstart start :tend end))))

(defun oi-get-client ()
  "Return a plist of client information with keys for :name :address1 :address2
:address3 :email and :phone."
  (oi-table->plist "Client"))

(defun oi-insert-invoice-header (ino)
  "Insert the header for the invoice generation buffer."
  (insert (format "#+TITLE: %s\n" ino))
  (insert "#+OPTIONS: toc:nil title:nil num:nil\n"
          (format "#+LATEX_CLASS: %s\n" oi-invoice-class)
          "#+LATEX_HEADER: \\usepackage{array}\n"
          "#+LATEX_HEADER: \\usepackage{tabularx}\n"
          (format "#+LATEX_HEADER: \\usepackage{%s}\n\n" oi-invoice-style)))

(defun oi-insert-company-header ()
  "insert the invoice header."
  (insert "#+begin_export: latex\n"
          "  \\hfil{\\Huge\\bf Invoice}\\hfil\\bigskip\\break\\hrule\n"
          (format "  %s \\hfill %s \\newline\n"
                  (plist-get oi-company :address1)
                  (plist-get oi-company :name))
          (format "  %s \\hfill %s \\newline\n"
                  (plist-get oi-company :address2)
                  (plist-get oi-company :email))
          (format "  %s \\hfill Phone: %s \\newline\n\\newline\n"
                  (plist-get oi-company :address3)
                  (plist-get oi-company :phone))
          "#+end_export:\n\n"))

(defun oi-insert-client-header (client)
  "Insert the invoice client header."
  (insert "#+begin_export: latex\n"
              "  \\begin{tabularx}{\\textwidth}{l X r}\n"
              "    \\begin{tabular}{l}\n"
              "      \\textbf{Invoice to} \\\\\n"
              (format "      \\ %s \\\\\n" (plist-get client :name))
              (format "      \\ %s \\\\\n" (plist-get client :address_1))
              (format "      \\ %s \\\\\n" (plist-get client :address_2))
              (format "      \\ %s \\\\\n" (plist-get client :address_3))
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
                      (plist-get client :terms))
              "     \\hline\n"
              "    \\end{tabular}\n"
              "  \\end{tabularx}\n"
              "#+end_export:\n\n"))

(defun oi-calc-amount (rate minutes)
  "Return amount based on hourly `rate' and number of `minutes'."
  (if (or (= rate 0.00) (= minutes 0))
      0.00
    (* minutes (/ rate 60))))

(defun oi-format-headline (headline level)
  "Format a 'headline' with amount of indent determined by the 'level'."
  (format " \\_ %s%s" (if (= level 3)
                          "  "
                        "")
          (org-shorten-string headline 40)))

(defun oi-clocktable-formatter (ipos tables params)
  "A custom clocktable formatter which adds columns for rate and amount.
Calculates amount due based on the rate set in 'oi-state'."
  (save-excursion
    (let* ((tbl (car tables))
          (entries (caddr tbl))
          (total-time (cadr tbl))
          (rate (plist-get oi-state :rate)))
      (setq oi-state (plist-put oi-state :amount (oi-calc-amount rate total-time)))
      (goto-char ipos)
      (insert (format "#+CAPTION: Clock summary at %s\n"
                      (format-time-string (org-time-stamp-format t t))))
      (insert "| Item |  | Time (h:m) | | Rate ($) | Amount ($) |\n"
              "|------+--+------------+-+----------+------------|\n")
      (pcase-dolist (`(,level ,headline ,tgs ,ts ,time ,props) entries)
        (when (> level 1)
          (insert (format "| %s | %s |\n"
                          (oi-format-headline headline level)
                          (if (= level 2)
                              (format "  | %s | | %.2f | %.2f "
                                      (org-duration-from-minutes (or time 0) 'h:mm)
                                      rate (oi-calc-amount rate (or time 0)))
                            (format "  |  | %s | | "
                                    (org-duration-from-minutes (or time 0) 'h:mm)))))))
      (insert "|----------+------+-+-+------+--------|\n"
              (format "| *Totals* |  | *%s* | | *%.2f* | *%.2f* |\n"
                      (org-duration-from-minutes (or total-time 0) 'h:mm)
                      (or rate 0.00)
                      (oi-calc-amount rate total-time))
              "|----------+------+-+-+------+--------|")
      (org-table-align))))

(defun oi-insert-clocktable (period scope)
  "insert a customized clock table which include rate and amount columns.
This function inserts the custom clocktable in the current buffer and adds
an additional table called 'totals' to calculate tax and final amount due.
Relies on data in the oi-state plist for rate, tax and tax_name."
  (let* ((formatter 'oi-clocktable-formatter)
        (org-clock-clocktable-default-properties
         (list :scope scope :maxlevel 3 :hidefiles t
               :tstart (plist-get period :tstart) :tend (plist-get period :tend)
               :formatter formatter)))
    (save-excursion
      (insert "#+NAME: services\n")
      (org-dynamic-block-insert-dblock "clocktable")
      (search-forward "#+END:")
      (forward-line)
      (insert "\n#+name: totals\n"
              "#+begin: table\n"
              "#+attr_latex: :environment tabularx :center nil :width \\textwidth :align X r\n"
              "|  | *Amount Due ($)* |\n"
              "|--+------------------|\n"
              (format "|  | %.2f  |\n" (plist-get oi-state :amount))
              "#+end:\n")
      (forward-line -1)
      (org-table-align))))

(defun oi-create-invoice (invoice-number client period path)
  "Creates a new invoice for the specified 'period'.
Uses the specified 'invoice-number' and data in the 'client' plist to generate
invoice details."
  (let ((inv-file (concat path invoice-number ".org"))
        (scope (list (buffer-name))))
    (save-current-buffer
      (find-file inv-file)
      (beginning-of-buffer)
      (oi-insert-invoice-header invoice-number)
      (oi-insert-company-header)
      (oi-insert-client-header client)
      (oi-insert-clocktable period scope)
      (save-buffer)
      inv-file)))

(defun oi-make-new-invoice ()
  "Generates a new invoice based on the current client org file.
The invoice period is determined by the 'START_PERIOD' property in the Invoice
entry. Client details are obtained from the Client table in the Client entry.
Will update the 'START_PERIOD' property to the current date and time so that
it becomes the start date for the next invoice run. Uses the TODO items in
the Task entry to determine times for the invoice. "
  (interactive)
  (let ((inv (oi-next-invoice-number))
        (client (oi-get-client)))
    (setq oi-state (plist-put oi-state :rate (string-to-number
                                              (plist-get client :rate))))
    (beginning-of-buffer)
    (search-forward "* Invoices")
    (org-insert-heading-after-current)
    (org-demote)
    (insert (format "%s\n\n" inv))
    (let* ((entry (point))
           (period (oi-invoice-period))
          (client-path (file-name-directory (buffer-file-name)))
          (i-file (oi-create-invoice inv client period client-path)))
      (org-entry-put entry "Invoice" inv)
      (org-entry-put entry "Start" (plist-get period :tstart))
      (org-entry-put entry "End" (plist-get period :tend))
      (org-entry-put entry "Rate" (format "%.2f" (plist-get oi-state :rate)))
      (insert (format "   Invoice: %s\n" inv))
      (insert (format "   Period %s to %s\n\n"
                      (plist-get period :tstart)
                      (plist-get period :tend)))
      (insert (format "   ORG: [[file:%s][%s]]\n" i-file inv))
      (insert (format "   PDF: [[file:%s%2$s.pdf][%2$s]]\n\n" client-path inv))
      (oi-insert-clocktable period 'file))))

(provide 'org-invoicing)
