(define-syntax timed-let
  (syntax-rules ()
    ((_ label (let-exp (vars/vals ...) body ...))
     (let-values (((ut1 st1) (cpu-time)))
       (let ((t1 (current-milliseconds)))
         (let-exp (vars/vals ...)
           (let-values (((ut2 st2) (cpu-time)))
             (let ((t2 (current-milliseconds)))
               (debug-message "~%~A Time [~A]: ~Ams / ~Ams / ~Ams ~%"  label  (logkey) (- ut2 ut1) (- st2 st1) (- t2 t1))
               body ...))))))))

(define-syntax timed
  (syntax-rules ()
    ((_ label body ...)
     (let-values (((ut1 st1) (cpu-time)))
       (let ((t1 (current-milliseconds)))
         (let ((result body ...))
           (let-values (((ut2 st2) (cpu-time)))
             (let ((t2 (current-milliseconds)))
               (debug-message "~%~A Time [~A]: ~Ams / ~Ams / ~Ams~%" label (logkey) (- ut2 ut1) (- st2 st1) (- t2 t1))
               result))))))))

(define-syntax timed-limit
  (syntax-rules ()
    ((_ limit label expression body ...)
     (let-values (((ut1 st1) (cpu-time)))
       (let ((t1 (current-milliseconds)))
         (let ((result body ...))
           (let-values (((ut2 st2) (cpu-time)))
             (let ((t2 (current-milliseconds)))
               (when (> (- ut2 ut1) limit)
                     (debug-message "~%Exceeded time limit for ~A [~A]: ~Ams / ~Ams / ~Ams~%~A~%~%"
                                    label (logkey) (- ut2 ut1) (- st2 st1) (- t2 t1)
                                    expression))
               result))))))))

(define-syntax try-safely
  (syntax-rules ()
    ((_ label exp body ...)
     (handle-exceptions exn 
                        (begin (log-message "~%==Error ~A== [~A]~%~A~%~%" 
                                            label (logkey) exp)
                               #f)
       body ...))))
