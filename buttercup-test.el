(require 'buttercup)

(describe "The buttercup-failed signal"
  (it "can be raised"
    (expect (lambda ()
              (signal 'buttercup-failed t))
            :to-throw
            'buttercup-failed)))

(describe "The buttercup-error signal"
  (it "can be raised"
    (expect (lambda ()
              (signal 'buttercup-error t))
            :to-throw
            'buttercup-error)))

(describe "The `expect' form"
  (it "with a matcher should translate directly to the function call"
    (expect (macroexpand '(expect (+ 1 1) :to-equal 2))
            :to-equal
            '(buttercup-expect (+ 1 1) :to-equal 2)))

  (it "with a form argument should extract the matcher from the form"
    (expect (macroexpand '(expect (equal (+ 1 1) 2)))
            :to-equal
            '(buttercup-expect (+ 1 1) #'equal 2)))

  (it "with a single argument should pass it to the function"
    (expect (macroexpand '(expect t))
            :to-equal
            '(buttercup-expect t))))

(describe "The `buttercup-expect' function"
  (describe "with a single argument"
    (it "should not raise an error if the argument is true"
      (expect (lambda ()
                (buttercup-expect t))
              :not :to-throw
              'buttercup-failed))

    (it "should raise an error if the argument is false"
      (expect (lambda ()
                (buttercup-expect nil))
              :to-throw
              'buttercup-failed
              "Expected nil to be non-nil")))

  (describe "with a function as a matcher argument"
    (it "should not raise an error if the function returns true"
      (expect (lambda ()
                (buttercup-expect t #'eq t))
              :not :to-throw
              'buttercup-failed))

    (it "should raise an error if the function returns false"
      (expect (lambda ()
                (buttercup-expect t #'eq nil))
              :to-throw
              'buttercup-failed)))

  (describe "with a matcher argument"
    (buttercup-define-matcher :always-true (a) t)
    (buttercup-define-matcher :always-false (a) nil)

    (it "should not raise an error if the matcher returns true"
      (expect (lambda ()
                (buttercup-expect 1 :always-true))
              :not :to-throw
              'buttercup-failed))

    (it "should raise an error if the matcher returns false"
      (expect (lambda ()
                (buttercup-expect 1 :always-false))
              :to-throw
              'buttercup-failed))))

(describe "The `buttercup-fail' function"
  (it "should raise a signal with its arguments"
    (expect (lambda ()
              (buttercup-fail "Explanation" ))
            :to-throw
            'buttercup-failed "Explanation")))

(describe "The `buttercup-define-matcher' macro"
  (it "should add a buttercup-matcher property"
    (buttercup-define-matcher :test-matcher (a b)
      (+ a b))

    (expect (funcall (get :test-matcher 'buttercup-matcher)
                     1 2)
            :to-equal
            3)))
