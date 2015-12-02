(require 'ox-latex)
(require 'org-loaddefs)

(defun link/org-export-table-to-subfigure (text backend info)
  "Convert table subfigure in LaTeX export."
  (when (org-export-derived-backend-p backend 'latex)
    (let* ((pt (next-property-change 0 text))
           (cell (plist-get (text-properties-at pt text) :parent))
           (table (org-export-get-parent-table cell))
           (attr (org-export-read-attribute :attr_latex table))
           (env (plist-get attr :environment))
           (continued (string-to-int (or (plist-get attr :continued) "0"))))
      (if (not (string= "subfigure" env))
          text
        (with-temp-buffer
          (insert text)
          (goto-char 1)
          (link/latex-table-to-subfigure continued)
          (buffer-string))))))


(defun link/latex-table-to-subfigure (continued)
  "Convert well-formed table to subfigure."
  (interactive "p")
  (let ((width ".9\\textwidth")
        (align "b")
        (option "")
        (count 0)
        (begend "")
        (centeringp nil)
        (template "\\begin{subfigure}[%s]{%s}
%s
\\caption{%s}
\\end{subfigure}\n"))
    (condition-case err
        (while t
          (beginning-of-line)
          (cond
           ;; \begin{table}[option], \end{table}
           ((looking-at "^\\\\\\(begin\\|end\\){table}")
            (let ((row (thing-at-point 'line t)))
              (when (string-match "^\\\\\\(begin\\|end\\){table}\\(\\[.*\\]\\)?" row)
                (setq begend (match-string 1 row))
                (setq option (or (match-string 2 row) ""))))
            (kill-whole-line)
            (insert (format "\\%s{figure}%s\n" begend option)))
           ;; \centering
           ((looking-at "^\\\\centering")
            (setq centeringp t)
            (next-line))
           ;; \begin{subfigure}{width}{align}, \begin{subfigure}{align}
           ;; \end{subfigure}
           ((looking-at "^\\\\\\(begin\\|end\\){subfigure}")
            (let ((row (thing-at-point 'line t))
                  maybe-width maybe-align)
              (when (string-match "{subfigure}\\({\\(.*?\\)}\\)?\\({\\(.*?\\)}\\)?" row)
                (setq maybe-width (match-string 2 row))
                (setq maybe-align (match-string 4 row))
                (unless maybe-align
                  (setq maybe-align maybe-width)
                  (setq maybe-width nil))
                (setq width (or maybe-width width))
                (setq align (or maybe-align align))))
            (kill-whole-line))
           ;; \toprule, \midrule, \bottomrule
           ((looking-at "^\\\\\\(top\\|mid\\|bottom\\)rule")
            (kill-whole-line))
           ;; table row
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
                  (setq count (1+ count))
                  (when (and (> continued 0)
                             (> count continued))
                    (setq count 0)
                    (insert (format "\\end{figure}\n
\\begin{figure}%s
\\ContinuedFloat\n" option))
                    (when centeringp
                      (insert "\\centering\n")))
                  (if (string-match "includegraphics" fig)
                      (insert (format template align width fig cap))
                    (if (string-match "includegraphics" cap)
                        (insert (format template align width cap fig))))))))
           ;; skip other
           (t (next-line))))
      (error (unless (equal (car err) 'end-of-buffer)
               (signal (car err) (cdr err)))))))

(provide 'ox-latex-subfigure)
