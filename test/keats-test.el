(ert-deftest terminating-keys ()
  (should (keats-terminating-key-p [13])) ;; RET
  (should (keats-terminating-key-p [7]))) ;; C-g

(ert-deftest non-terminating-keys ()
  (should-not (keats-terminating-key-p [16]))    ;; C-p
  (should-not (keats-terminating-key-p [3 13]))) ;; C-c RET

(ert-deftest valid-keats ()
  (let ((keat (make-keats-keat :key "C-x b" :description "Valid")))
    (should (keats-valid-keat-p keat))))

(ert-deftest invalid-keats ()
  (let ((keats
         (list
          (make-keats-keat :key ""      :description "Valid")
          (make-keats-keat :key "C-x b" :description "")
          nil)))
    (dolist (keat keats)
      (should-not (keats-valid-keat-p keat)))))

(ert-deftest prefix-key ()
  (should (equal "C-c k" keats-prefix-key)))

(ert-deftest read-description ()
  (with-mock
   (stub read-string => "description")
   (should (equal "description" (keats-read-description)))))

(ert-deftest read-key-no-key ()
  (with-mock
   (stub keats-read-key => nil)
   (should-not (keats-read-keat))))

(ert-deftest read-key-with-key ()
  (with-mock
   (stub keats-read-key => "C-x b")
   (stub keats-read-description => "description")
   (let* ((keat (keats-read-keat))
          (key (keats-keat-key keat))
          (description (keats-keat-description keat)))
     (should (keats-keat-p keat))
     (should (equal "C-x b" key))
     (should (equal "description" description)))))

(ert-deftest exists-key-does-not-exist ()
  (preserve-keats
   (keats-add (make-keats-keat :key "C-x b"))
   (should-not (keats-exists-p "C-x C-b"))))

(ert-deftest exists-key-does-exist ()
  (preserve-keats
   (keats-add (make-keats-keat :key "C-x b"))
   (should (keats-exists-p "C-x b"))))

(ert-deftest add-keat ()
  (preserve-keats
   (keats-add (make-keats-keat :key "C-x b" :description "description"))
   (let* ((keat (car keats-list))
          (key (keats-keat-key keat))
          (description (keats-keat-description keat)))
     (should (equal 1 (length keats-list)))
     (should (equal "C-x b" key))
     (should (equal "description" description)))))

(defmacro preserve-keats (&rest body)
  "Executes body without messing with `keats-list'."
  `(let ((keats-list))
     ,@body))
