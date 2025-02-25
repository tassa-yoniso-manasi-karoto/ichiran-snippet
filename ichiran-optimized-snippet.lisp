  (progn
    (ql:quickload :jsown :silent t)
    
    ;; Define custom to-json method to include match-readings data
    (defmethod jsown:to-json ((word-info ichiran/dict::word-info))
      (let* ((gloss-json (handler-case
                            (ichiran::word-info-gloss-json word-info)
                          (error (e) (declare (ignore e)) nil)))
             (match-json (handler-case
                            (ichiran/kanji:match-readings-json
                              (slot-value word-info (quote ichiran/dict::text))
                              (slot-value word-info (quote ichiran/dict::kana)))
                          (error (e) (declare (ignore e)) nil)))
             
             ;; Get the basic JSON structure from word-info-json
             (word-json (ichiran::word-info-json word-info)))
        
        ;; Add gloss data if available (includes translations, conj info, etc.)
        (when gloss-json
          (jsown:extend-js word-json ("gloss" gloss-json)))
        
        ;; Add match-readings data 
        (when match-json
          (jsown:extend-js word-json ("match" match-json)))
        
        ;; Return the enhanced JSON
        (jsown:to-json word-json)))
    
    ;; Use the built-in romanize* function with our custom to-json method
    (jsown:to-json (ichiran::romanize* "%%s" :limit 1)))
