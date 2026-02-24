(defun c:G6 ( / pt angList idx done lenStr len gr key nextpt
                prevPt2 lastLen finishedInput segStack
                scaleFactor oldOsmode oldCmdecho *error* dimOff
                segList seg userLen userVal)

  (defun *error* (msg)
    (if oldOsmode  (setvar "OSMODE"  oldOsmode))
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (defun deg->rad (d) (* pi (/ d 180.0)))
  (defun rad->deg (r) (* 180.0 (/ r pi)))

  ;; segment record helpers (alist)
  (defun make-seg (p1 p2 lineEnt drawLen userLen)
    (list (cons 'p1 p1)
          (cons 'p2 p2)
          (cons 'lineEnt lineEnt)
          (cons 'drawLen drawLen)
          (cons 'userLen userLen)
          (cons 'dimEnt nil))
  )
  (defun seg-get (k seg) (cdr (assoc k seg)))

  (setq oldOsmode  (getvar "OSMODE")
        oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (setq angList (mapcar 'deg->rad '(30 90 150 -150 -90 -30))
        idx 0
        scaleFactor 0.01
        segStack '())

  (defun updatePreview ( / curAng curLen curEnd userLen)
    (if prevPt2 (grdraw pt prevPt2 0))
    (setq curAng (nth idx angList))
    (if (= lenStr "")
      (setq curLen (if lastLen lastLen 1.0))
      (progn
        (setq userLen (abs (atof lenStr)))
        (setq curLen (* scaleFactor userLen))))
    (setq curEnd (polar pt curAng curLen))
    (grdraw pt curEnd 1)
    (setq prevPt2 curEnd))

  (setq pt (getpoint "\nStart point: "))
  (if pt
    (progn
      (setvar "OSMODE" 0)
      (setq done nil lastLen 1.0 prevPt2 nil)

      (while (not done)
        (setq lenStr "" finishedInput nil)
        (prompt
          (strcat
            "\nCurrent angle = "
            (rtos (rad->deg (nth idx angList)) 2 0)
            "  | Enter length (scale 0.01; TAB = angle, U = undo last, ENTER = finish): "))
        (updatePreview)

        (while (not finishedInput)
          (setq gr (grread T 1 0))
          (cond
            ((= (car gr) 2)
             (setq key (cadr gr))
             (cond
               ((= key 9)
                (setq idx (1+ idx))
                (if (>= idx (length angList)) (setq idx 0))
                (prompt (strcat "\nAngle: " (rtos (rad->deg (nth idx angList)) 2 0)))
                (updatePreview))

               ((= key 13)
                (if (= lenStr "")
                  (progn
                    (if prevPt2 (grdraw pt prevPt2 0))
                    (setq prevPt2 nil done T finishedInput T))
                  (progn
                    (setq userLen (abs (atof lenStr))
                          len (* scaleFactor userLen))
                    (if prevPt2 (grdraw pt prevPt2 0))
                    (setq prevPt2 nil)
                    (setq nextpt (polar pt (nth idx angList) len))
                    (command "_.LINE" pt nextpt "")
                    (setq segStack (cons (make-seg pt nextpt (entlast) len userLen) segStack)
                          pt nextpt
                          lastLen len
                          finishedInput T)
                    (command "_.REGEN"))))

               ((or (= key 85) (= key 117))
                (if segStack
                  (progn
                    (if prevPt2 (grdraw pt prevPt2 0))
                    (setq prevPt2 nil
                          seg (car segStack)
                          segStack (cdr segStack))
                    (if (seg-get 'lineEnt seg) (entdel (seg-get 'lineEnt seg)))
                    (setq pt (seg-get 'p1 seg)
                          lastLen (seg-get 'drawLen seg))
                    (prompt "\nLast segment undone.")
                    (updatePreview)
                    (command "_.REGEN"))
                  (prompt "\nNo segment to undo.")))

               ((= key 8)
                (if (> (strlen lenStr) 0)
                  (progn
                    (setq lenStr (substr lenStr 1 (1- (strlen lenStr))))
                    (prompt (strcat "\rLength (input): " lenStr " "))
                    (updatePreview))))

               ((and (>= key 48) (<= key 57))
                (setq lenStr (strcat lenStr (chr key)))
                (prompt (strcat "\rLength (input): " lenStr))
                (updatePreview))

               ((= key 46)
                (setq lenStr (strcat lenStr "."))
                (prompt (strcat "\rLength (input): " lenStr))
                (updatePreview))

               ((= key 45)
                (prompt "\nNegative length is not allowed; value kept positive.")))))))

      (setvar "OSMODE" oldOsmode)

      (if segStack
        (progn
          (setq dimOff (getdist "\nDimension offset distance <Enter for no dimensions>: "))
          (if dimOff
            (progn
              (setq segList (reverse segStack))
              (while segList
                (setq seg (car segList)
                      userVal (seg-get 'userLen seg)
                      segList (cdr segList))
                (if (> userVal 25.0)
                  (progn
                    (setq p1 (seg-get 'p1 seg)
                          p2 (seg-get 'p2 seg))
                    (if (and p1 p2)
                      (progn
                        (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
                              ang (angle p1 p2)
                              nAng (+ ang (/ pi 2.0))
                              dimPt (polar mid nAng dimOff))
                        (command "_.DIMALIGNED" p1 p2 dimPt ""))))))))))))

  (setvar "OSMODE"  oldOsmode)
  (setvar "CMDECHO" oldCmdecho)
  (princ))
