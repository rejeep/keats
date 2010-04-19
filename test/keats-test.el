(ert-deftest terminating-keys ()
  (should (terminating-key-p [13])) ;; RET
  (should (terminating-key-p [7]))  ;; C-g)

(ert-deftest non-terminating-keys ()
  (should-not (terminating-key-p [16]))   ;; C-p
  (should-not (terminating-key-p [3 13])) ;; C-c RET)
