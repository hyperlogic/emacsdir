(require 'cl-lib)

(defun ajt-quat-to-axis-angle (q)
  (let ((qx (car q))
        (qy (cadr q))
        (qz (caddr q))
        (qw (cadddr q)))
    (let ((angle (* 2.0 (acos qw)))
          (s (- 1.0 (* qw qw))))
      (if (<= s 0.0)
          (list 1.0 0.0 0.0 angle)
        (list (/ qx (sqrt s))
              (/ qy (sqrt s))
              (/ qz (sqrt s))
              angle)))))

(defun ajt-vec-dot (a b)
  (cl-reduce '+ (mapcar* '* a b)))

(defun ajt-vec-len (v)
  (sqrt (ajt-vec-dot v v)))

(defun ajt-vec-add (a b)
  (mapcar* '+ a b))

(defun ajt-vec3-sub (a b)
  (mapcar* '- a b))

(defun ajt-vec-mul (a b)
  (mapcar* '* a b))

(defun ajt-vec-x (v)
  (car v))

(defun ajt-vec-y (v)
  (cadr v))

(defun ajt-vec-z (v)
  (caddr v))

(defun ajt-vec-w (v)
  (cadddr v))

(defun ajt-vec3-normalize (v)
  (let ((len (/ 1.0 (ajt-vec-len v))))
    (ajt-vec-mul (list len len len) v)))

(defun ajt-quat-normalize (v)
  (let ((len (/ 1.0 (ajt-vec-len v))))
    (ajt-vec-mul (list len len len len) v)))

(defun ajt-lerp (a b alpha)
  (+ (* (- 1 alpha) a) (* alpha b)))


