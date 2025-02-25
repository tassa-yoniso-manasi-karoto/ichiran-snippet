  (progn
    (ql:quickload :jsown :silent t)
    
    ;; Function to create a JSON representation of a word with match-readings
    (defun make-word-json (word)
      (let* ((text (slot-value word (quote ichiran/dict::text)))
             (kana (slot-value word (quote ichiran/dict::kana)))
             (word-score (slot-value word (quote ichiran/dict::score)))
             (word-type (slot-value word (quote ichiran/dict::type)))
             (seq (slot-value word (quote ichiran/dict::seq)))
             (conj (slot-value word (quote ichiran/dict::conjugations)))
             (components (slot-value word (quote ichiran/dict::components)))
             (start (slot-value word (quote ichiran/dict::start)))
             (end (slot-value word (quote ichiran/dict::end)))
             
             ;; Get gloss JSON with translation/definition information
             (gloss-json (handler-case 
                            (ichiran::word-info-gloss-json word)
                          (error (e) (declare (ignore e)) nil)))
             
             ;; Create basic token
             (token-json (jsown:new-js
                           ("text" text)
                           ("kana" kana)
                           ("score" word-score)
                           ("type" word-type)
                           ("start" start)
                           ("end" end))))
        
        ;; Add seq if available
        (when seq
          (jsown:extend-js token-json ("seq" seq)))
        
        ;; Add gloss if available
        (when gloss-json
          (jsown:extend-js token-json ("gloss" gloss-json)))
        
        ;; Add conjugation info if available
        (when (and conj (not (eql conj :root)))
          (jsown:extend-js token-json ("conj" conj)))
        
        ;; Add compound word info if available
        (when components
          (jsown:extend-js token-json 
                           ("compound" 
                            (mapcar (lambda (comp) 
                                      (slot-value comp (quote ichiran/dict::text)))
                                    components))))
        
        ;; Add match-readings info - kanji to kana mappings
        (let ((match-json (handler-case 
                             (ichiran/kanji:match-readings-json text kana)
                           (error (e) (declare (ignore e)) nil))))
          (when match-json
            (jsown:extend-js token-json ("match" match-json))))
        
        ;; Return the token object
        token-json))
    
    ;; Main function - direct use of dict-segment, avoiding romanize*
    (jsown:to-json
      (let* ((input "図書館で、私は専門的な歴史文献を詳細に分析し、複雑な国際関係の変遷を深く理解しようと試みました。")
             (normalized-input (ichiran::normalize input))
             (segments (ichiran::dict-segment normalized-input :limit 1))
             (first-segment (car segments))
             (words (car first-segment))
             (score (cdr first-segment)))
        
        ;; Process each word, creating the exact structure needed
        (list
          (list
            (mapcar 
              (lambda (word)
                ;; Format: [romanized, word-json, []]
                (list
                  ;; Get romanized form
                  (handler-case
                    (ichiran::romanize-word-info word :method ichiran::*hepburn-traditional*)
                    (error (e) (declare (ignore e)) nil))
                  ;; Get word JSON with all data
                  (make-word-json word)
                  ;; Empty third element to match expected format
                  (list)))
              words)
            score)))))
