(require 'ox-latex)
(require 'org-loaddefs)

(defun link/org-export-table-to-subfigure (text backend info)
  "Convert table subfigure in LaTeX export."
  (when (org-export-derived-backend-p backend 'latex)
    (let* ((pt (next-property-change 0 text))
           (cell (plist-get (text-properties-at pt text) :parent))
           (table (org-export-get-parent-table cell))
           (attr (org-export-read-attribute :attr_latex table))
           (env (plist-get attr :environment)))
      (if (not (string= "subfigure" env))
          text
        (with-temp-buffer
          (insert text)
          (goto-char 1)
          (link/latex-table-to-subfigure)
          (buffer-string))))))


(defun link/latex-table-to-subfigure ()
  "Convert well-formed table to subfigure."
  (interactive)
  (let ((width ".9\\textwidth")
        (align "b")
        (template "\\begin{subfigure}[%s]{%s}
%s
\\caption{%s}
\\end{subfigure}\n"))
    (condition-case err
        (while t
          (beginning-of-line)
          (cond ((looking-at "^\\\\\\(begin\\|end\\){table}")
                 (re-search-forward "table" nil t)
                 (replace-match "figure")
                 (next-line))
                ((looking-at "^\\\\\\(begin\\|end\\){subfigure}")
                 (let ((row (thing-at-point 'line t)))
                   (when (string-match "{subfigure}\\({\\(.*?\\)}\\)?\\({\\(.*?\\)}\\)?" row)
                     (setq width (match-string-no-properties 2 row))
                     (setq align (match-string-no-properties 4 row))))
                 (kill-whole-line))
                ((looking-at "^\\\\\\(top\\|bottom\\)rule")
                 (kill-whole-line))
                ((looking-at ".*\\\\\\\\$")
                 (let ((row1 nil)
                       (row2 nil)
                       (cell1 nil)
                       (cell2 nil))
                   (setq row1 (thing-at-point 'line t))
                   (kill-whole-line)
                   (setq row2 (thing-at-point 'line t))
                   (kill-whole-line)
                   (setq row1 (replace-regexp-in-string "\\\\\\\\\n$" "" row1))
                   (setq row2 (replace-regexp-in-string "\\\\\\\\\n$" "" row2))
                   (setq cell1 (split-string row1 " & "))
                   (setq cell2 (split-string row2 " & "))
                   (dolist (cell (cl-mapcar #'cons cell1 cell2))
                     (let ((fig (car cell))
                           (cap (cdr cell)))
                       (if (string-match "includegraphics" fig)
                           (insert (format template align width fig cap))
                         (if (string-match "includegraphics" cap)
                             (insert (format template align width cap fig))))))))
                (t (next-line))))
      (error (unless (equal (car err) 'end-of-buffer)
               (signal (car err) (cdr err)))))))

(provide 'ox-latex-subfigure)
