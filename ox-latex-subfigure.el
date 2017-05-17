(require 'ox-latex)
(require 'org-loaddefs)

(defun link/org-export-table-to-subfigure (text backend info)
  "Convert table to subfigure in LaTeX export."
  (when (org-export-derived-backend-p backend 'latex)
    (if (not (next-property-change 0 text))
        text
      (let ((pt 0)
            cell table attr env limit)
        (while (or (not table)
                   (not pt))
          (setq pt (next-property-change pt text)
                cell (plist-get (text-properties-at pt text) :parent)
                table (org-export-get-parent-table cell)))
        (setq attr (org-export-read-attribute :attr_latex table)
              env (plist-get attr :environment)
              limit (string-to-int (or (plist-get attr :limit) "0")))
        (if (not (string= "subfigure" env))
            text
          (with-temp-buffer
            (insert text)
            (goto-char 1)
            (link/latex-table-to-subfigure limit)
            (buffer-string)))))))

(defun link/latex-table-to-subfigure (limit)
  "Convert well-formed table to subfigure."
  (interactive "p")
  (let ((width ".9\\textwidth")
        (align "b")
        (option "")
        (centering "")
        (caption "")
        fig cap)
    (beginning-of-line)
    (while (not (looking-at "^\\\\end{table}"))
      (let ((row (thing-at-point 'line t)))
        (kill-whole-line)
        (cond
         ;; \begin{table}[option], \end{table}
         ((string-match "^\\\\begin{table}\\(\\[.*\\]\\)?" row)
          (setq option (or (match-string 1 row) "")))
         ;; \centering
         ((string-match "^\\\\centering" row)
          (setq centering "\\centering\n"))
         ;; \begin{subfigure}{width}{align}, \begin{subfigure}{align}
         ;; \end{subfigure}
         ((string-match "^\\\\begin{subfigure}\\({\\(.*?\\)}\\)?\\({\\(.*?\\)}\\)?" row)
          (let (maybe-width maybe-align start-of-table row-start row-end)
            (setq maybe-width (match-string 2 row))
            (setq maybe-align (match-string 4 row))
            (unless maybe-align
              (setq maybe-align maybe-width)
              (setq maybe-width nil))
            (setq width (or maybe-width width))
            (setq align (or maybe-align align))

            (setq start-of-table (point))

            ;; Remove all possible rules or hlines
            (setq rules
                  (mapconcat 'identity
                             '("hline" "vline" "toprule" "midrule" "bottomrule")
                             "\\|"))
            (while (re-search-forward (concat "\\\\\\(" rules  "\\)\n?") nil t)
              (replace-match ""))

            (goto-char start-of-table)

            ;; Transform all multi line rows to single line rows by replacing
            ;; newlines with spaces
            (while (not (looking-at "\\\\end{subfigure}"))
              (setq row-start (point))

              ;; Search for a new row delimiter
              (when (re-search-forward "\\\\\\\\\n?" nil t)
                (goto-char row-start)

                (setq row-end (match-beginning 0))

		;; Replace all newlines in the row with spaces
                (save-match-data
                  (while (re-search-forward "\n" row-end t)
                    (replace-match " ")))

                (setq row-start (match-end 0))
                (goto-char row-start)))

            ;; Go back the start of the table and continue with normal
            ;; row formatting
            (goto-char start-of-table)))
         ;; \caption{\label{tab:orgtable1}
         ;; xxx}
         ((string-match "^\\\\caption[\\\\[{]" row)
          (setq caption (concat row (thing-at-point 'line t)))
          (kill-whole-line))
         ;; table row
         ((string-match ".*\\\\\\\\$" row)
          (let ((striped-row (replace-regexp-in-string "\\\\\\\\\n$" "" row)))
            (setq fig (append fig (split-string striped-row " & ")))
            (setq row (thing-at-point 'line t))
            (kill-whole-line)
            (setq striped-row (replace-regexp-in-string "\\\\\\\\\n$" "" row))
            (setq cap (append cap (split-string striped-row " & "))))))))
    (kill-whole-line)
    (insert (format "\\begin{figure}%s\n%s%s" option
                    (if org-latex-caption-above caption "")
                    centering))
    (dotimes (i (length fig))
      (let ((f (nth i fig))
            (c (nth i cap)))
        (when (string-match "includegraphics" c)
          (setq f (prog1 c (setq c f))))
        (when (string-match "\\(\\\\includegraphics\\(:?\\[.*?\\]\\)?{.*?}\\)" f)
          (setq f (match-string 1 f))
          (insert (format "\\begin{subfigure}[%s]{%s}
%s
\\caption{%s}
\\end{subfigure}\n" align width f c))
          (when (and (> limit 0)
                     (< i (1- (length fig)))
                     (= (mod (1+ i) limit) 0))
            (insert (format "\\end{figure}\n
\\begin{figure}%s
%s\\ContinuedFloat\n" option centering))))))
    (insert (format "%s\\end{figure}\n"
                    (if org-latex-caption-above "" caption)))))

(add-hook 'org-export-filter-table-functions
          'link/org-export-table-to-subfigure)

(provide 'ox-latex-subfigure)
