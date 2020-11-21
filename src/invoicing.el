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

(defcustom oi-invoice-style "~/.spacemacs.d/invoice/invoice.sty"
  "Location of the invoice.sty Latex style file."
  :type 'file
  :group 'org-invoicing)

(defvar oi-state '(:rate 0.00 :tax-rate 0.00 :tax-name "GST" :tax-amount 0.00
                         :expense-amount 0.00 :amount 0.00 :total 0.00)
  "Variable which holds current state of invoice generator.")

(defun string->keyword (s)
  "Turn the string 's' into a keyword.
The string will have spaces replaced with '_' and all will be all lower case."
  (intern (format ":%s" (substitute ?_ ?  (downcase s)))))

(defun oi-reset-state (rate tax-rate tax-name)
  (setq oi-state (list :rate rate :tax-rate tax-rate :tax-name tax-name
                       :tax-amount 0.00 :expense-amount 0.00 :amount 0.00
                       :total 0.00)))

(defun oi-update-state (key val)
  (setq oi-state (plist-put oi-state key val)))

(defun oi-get-state (key)
  (plist-get oi-state key))

(defun oi-table->list (table-name)
  "Return a list containing lists representing each row in a table.
The string elements are the cells for each row. The `table-name' argument
specifies a unique table name to identify the table source."
  (save-excursion
    (goto-char (point-min))
    (search-forward (format "#+name: %s" table-name))
    (forward-line 1)
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
    (goto-char (point-min))
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
    (goto-char (point-min))
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
          (format "#+LATEX_HEADER: \\usepackage{%s}\n\n"
                  (file-name-sans-extension (expand-file-name oi-invoice-style)))))

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
          (rate (oi-get-state :rate)))
      (oi-update-state :amount (oi-calc-amount rate total-time))
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
    (insert "#+name: services\n")
    (org-dynamic-block-insert-dblock "clocktable")
    (search-forward "#+END:")
    (forward-line 1)))

(defun oi-insert-totals-table ()
  "Insert the table of total charges."
  (message "oi-i-totals-table: %s" oi-state)
  (when (< 0 (oi-get-state :tax-rate))
    (oi-update-state :tax-amount (* (oi-get-state :tax-rate)
                                    (oi-get-state :amount))))
  (oi-update-state :total (+ (oi-get-state :amount)
                             (oi-get-state :tax-amount)
                             (oi-get-state :expense-amount)))
  (insert "\n#+name: totals\n"
          "#+begin: table\n"
          "#+attr_latex: :environment tabularx :center nil :width \\textwidth :align X l r\n"
          "|  |                  |\n"
          "|--+------------------|\n"
          (format "|  | *Services* | %.2f  |\n" (oi-get-state :amount))
          (format "| | *%s* | %.2f |\n" (oi-get-state :tax-name) (oi-get-state :tax-amount)))
  (when (< 0 (oi-get-state :expense-amount))
    (insert (format "| | *Expenses* | %.2f |\n" (oi-get-state :expense-amount))))
  (insert (format "| | *Total Due* | %.2f |\n" (oi-get-state :total))
          "#+end:\n")
  (forward-line -1)
  (org-table-align))

(defun oi-get-expense-data ()
  "Gather list of unclaimed expenses from current entry."
  (let* ((entry (point))
         (desc (org-entry-get entry "Description"))
         (date (org-entry-get entry "Date"))
         (amount (org-entry-get entry "Amount")))
    (org-toggle-tag "CLAIMED")
    (message "oi-get-eexpense-data Entry: %s Desc: %s Date: %s Amount: %s" entry desc date amount)
    (list :date date :description desc :amount amount)))

(defun oi-gather-expenses ()
  "Gather unclaimed expense data to add to invoice."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "* Expenses")
    (let ((data (org-map-entries 'oi-get-expense-data "+EXPENSE-CLAIMED" 'tree)))
      (message "oi-gather-expenses: data: %s" data)
      data)))

(defun oi-insert-expenses-table (exp)
  "Insert the expenses table. The `exp' argument is a list of plists."
  (insert "#+name: expenses\n"
          "| *Date* | *Item* | *Amount* |\n"
          "|--------+--------+----------|\n")
  (seq-do (lambda (e)
            (message "seq-do e: %s" e)
            (let ((date (plist-get e :date))
                  (desc (plist-get e :description))
                  (amount (plist-get e :amount)))
              (insert "| %s | %s | %.2f |\n" date desc amount)
              (oi-update-state :expense-amount (+ (oi-get-state :expense-amount) amount))))
          exp)
  (insert (format "| | *Total Due* | %.2f |\n" (oi-get-state :expense-amount))))

(defun oi-create-invoice (invoice-number client period path &optional expense-data)
  "Creates a new invoice for the specified 'period'.
Uses the specified 'invoice-number' and data in the 'client' plist to generate
invoice details."
  (let ((scope (list (buffer-name)))
        (inv-file (concat path invoice-number ".org")))
    (save-current-buffer
      (find-file inv-file)
      (goto-char (point-min))
      (oi-insert-invoice-header invoice-number)
      (oi-insert-company-header)
      (oi-insert-client-header client)
      (oi-insert-clocktable period scope)
      (when expense-data
        (message "oi-c-invoice expense data: %s" expense-data)
        (oi-insert-expenses-table expense-data))
      (oi-insert-totals-table)
      (org-latex-export-to-pdf))))

(defun oi-make-new-invoice ()
  "Generates a new invoice based on the current client org file.
The invoice period is determined by the 'START_PERIOD' property in the Invoice
entry. Client details are obtained from the Client table in the Client entry.
Will update the 'START_PERIOD' property to the current date and time so that
it becomes the start date for the next invoice run. Uses the TODO items in
the Task entry to determine times for the invoice. "
  (interactive)
  (let ((inv (oi-next-invoice-number))
        (period (oi-invoice-period))
        (exp-data (oi-gather-expenses))
        (client (oi-get-client)))
    (oi-reset-state (string-to-number (or (plist-get client :rate) 0.00))
                    (string-to-number (or (plist-get client :tax) 0.00))
                    (or (plist-get client :tax_name) "Tax"))
    (goto-char (point-min))
    (search-forward "* Invoices")
    (org-insert-heading-after-current)
    (org-demote)
    (insert (format "%s\n\n" inv))
    (let* ((entry (point))
           (client-path (file-name-directory (buffer-file-name))))
      (oi-create-invoice inv client period client-path exp-data)
      (org-entry-put entry "Invoice" inv)
      (org-entry-put entry "Start" (plist-get period :tstart))
      (org-entry-put entry "End" (plist-get period :tend))
      (org-entry-put entry "Rate" (format "%.2f" (oi-get-state :rate)))
      (insert (format "   Invoice: %s\n" inv))
      (insert (format "   Period %s to %s\n\n"
                      (plist-get period :tstart)
                      (plist-get period :tend)))
      (insert (format "   PDF: [[file:%s%2$s.pdf][%2$s]]\n\n" client-path inv))
      (oi-insert-clocktable period 'file)
      (when exp-data
        (oi-insert-expense-table exp-data)))))

(provide 'org-invoicing)
