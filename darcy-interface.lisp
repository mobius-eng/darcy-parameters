(in-package darcy-interface)

(in-readtable :qtools)

(define-widget results-table (QDialog)
  ((simulation
    :initarg :simulation
    :documentation
    "Simulation object"))
  (:documentation "Results dialog with the table"))

(define-subwidget (results-table table)
    (q+:make-qtablewidget)
  (with-accessors ((darcy darcy-simulation-model)
                   (results darcy-simulation-results)) simulation
    (let ((mesh-points (simulation-results-mesh-points results))
          (times (simulation-results-time results))
          (saturation (simulation-results-saturation results)))
      (q+:set-row-count table (+ 2 (length mesh-points)))
      (q+:set-column-count table (+ 1 (length times)))
      ;; Fill out the headers
      (let ((depth-title (q+:make-qtablewidgetitem "Depth, m"))
            (time-title (q+:make-qtablewidgetitem "Time, h")))
        (q+:set-text-alignment depth-title (+ (q+:qt.align-bottom) (q+:qt.align-hcenter)))
        (q+:set-text-alignment time-title (q+:qt.align-hcenter))
        (q+:set-item table 0 0 depth-title)
        (q+:set-item table 0 1 time-title)
        (loop for time across times
           for column from 1
           do (q+:set-item table 1 column
                           (q+:make-qtablewidgetitem
                            (format nil "~,1F" (/ time 3600d0))))))
      ;; Fill out the data
      (loop for point across mesh-points
         for space-index from 0
         for table-row from 2
         do (progn
              (q+:set-item
               table table-row 0
               (q+:make-qtablewidgetitem (format nil "~,3F" point)))
              (loop for table-column from 1
                 for time-index from 0 below (length times)
                 do (q+:set-item
                     table table-row table-column
                     (q+:make-qtablewidgetitem
                      (format nil "~,5,,,,,'eG"
                              (aref saturation time-index space-index)))))))
      ;; Set the span of Depth
      (q+:set-span table 0 0 2 1)
      ;; Set the span of Time
      (q+:set-span table 0 1 1 (length times)))))


(define-subwidget (results-table layout)
    (q+:make-qgridlayout results-table)
  (q+:add-widget layout table 0 0 1 1))

(defmethod initialize-instance :after ((dialog results-table) &key)
  (q+:set-modal dialog nil)
  (q+:set-window-title dialog "Simulation results"))

(defun output-data-csv (results out
                        &optional
                          (transformer #'aref)
                          (format "~,5,,,,,'eG")
                          (time-row-format "~,1F"))
  (let* ((mesh-points (simulation-results-mesh-points results))
         (times (simulation-results-time results))
         (saturation (simulation-results-saturation results))
         (output-values
          (loop for time across times
             for time-index from 0
             collect (let ((saturation-column (slice saturation time-index t)))
                       (loop for x across mesh-points
                          for space-index from 0
                          collect (funcall transformer saturation-column space-index)))))
         (output-format (format nil "~~,3F,~~{~A~~^,~~}~~%" format))
         (header-row-format (format nil "~~A,~~{~A~~^,~~}~~%" time-row-format)))
    ;; Output header-row
    (format out header-row-format "Depth"
            (map 'list (lambda (x) (/ x 3600d0)) times))
    ;; Output data
    (loop for point across mesh-points
       for space-index from 0
       do (progn
            (format out output-format
                    point
                    (loop for vec in output-values
                       collect (elt vec space-index)))))))

(defun output-average-saturation-csv (results out
                                      &optional (format "~,5,,,,,'eG"))
  (let ((average-saturation (column-average-saturation results))
        (output-format (format nil "~~,3F,~A~~%" format))
        (times (simulation-results-time results)))
    (format out "Time (h), Average saturation~%")
    (loop for time across times
       for s across average-saturation
       do (format out output-format (/ time 3600d0) s))))

(defun save-data-csv (location name results
                      &key
                        (transformer #'aref)
                        (format "~,5,,,,,'eG")
                        (time-row-format "~,1F"))
  (with-open-file (out
                   (make-pathname :name name :type "csv"
                                  :defaults location)
                   :direction :output
                   :if-exists :supersede)
    (output-data-csv results out transformer format time-row-format)))

(defvar *plot-average-saturation-script*
  "reset
set terminal png enhanced size 600,400 font \"InputMono\" 10
set output 'average-saturation.png'
# set termoption dashed
set datafile separator ','

set title \"Evolution of average saturation over time\"
set xlabel \"Elapsed time, h\"
set ylabel \"Effective saturation, -\"

unset grid

unset key

plot \"average-saturation.csv\" using 1:2 with lines")


(defvar *plot-header*
  "reset
set terminal png enhanced size 800,600 font \"InputMono\" 10
set output 'summary.png'
set multiplot layout 2, 2 title 'Model summary'
set tmargin 2
set termoption dashed
set datafile separator ','
unset grid

")

(defvar *plot-saturation*
  "
unset yrange
unset xrange

set key autotitle columnhead center bmargin horizontal

set xlabel \"Depth, m\"
set ylabel \"Effective saturation, -\"

set title 'Saturation'

plot")

(defvar *plot-conductivity*
  "
unset yrange
unset xrange

set key autotitle columnhead center bmargin horizontal

set xlabel \"Depth, m\"
set ylabel \"Conductivity, m/s\"
set title 'Conductivity'

plot")

(defvar *plot-pressure*
  "
unset yrange
unset xrange

set key autotitle columnhead center bmargin horizontal

set xlabel \"Depth, m\"
set ylabel \"Pressure, m\"
set title 'Capillary pressure'

plot")



(defvar *plot-footer*
  "
unset multiplot
")


(defun run-gnuplot (location script)
  (let ((current-location (uiop/os:getcwd)))
    (uiop/os:chdir location)
    (uiop/run-program:run-program (format nil "gnuplot ~A" script))
    (uiop/os:chdir current-location)))

(defun save-script (location script name)
  (let ((script-file (make-pathname :name name :type "gpl"
                                    :defaults location)))
    (with-open-file (out script-file :direction :output :if-exists :supersede)
      (format out "~A~%" script))
    (run-gnuplot location (format nil "~A.gpl" name))))

(defun make-summary-script (max-column step
                            &optional
                              (satfile "saturation-plot-table.csv")
                              (condfile "conductivity-plot-table.csv")
                              (presfile "pressure-plot-table.csv"))
  (with-output-to-string (out)
    (format out "~A~%" *plot-header*)
    (format out "~A " *plot-saturation*)
    (format out "~{~S using 1:~D with lines~^,\\~%~T~}"
            (loop for column from 2 upto max-column by step
               collect satfile
               collect column))
    (format out "~%~A " *plot-conductivity*)
    (format out "~{~S using 1:~D with lines~^,\\~%~T~}"
            (loop for column from 2 upto max-column by step
               collect condfile
               collect column))
    (format out "~%~A " *plot-pressure*)
    (format out "~{~S using 1:~D with lines~^,\\~%~T~}"
            (loop for column from 2 upto max-column by step
               collect presfile
               collect column))
    (format out *plot-footer*)))

(defun save-plot-scripts (simulation location)
  (let* ((results (darcy-simulation-results simulation))
         (times (simulation-results-time results))
         (output-time-interval (darcy-simulation-output-time-interval simulation))
         (plot-time-interval (darcy-simulation-plot-time-interval simulation))
         (plot-step (ceiling plot-time-interval output-time-interval))
         (times-count (length times)))
    (save-script location
                 (make-summary-script (1+ times-count) plot-step)
                 "plot-summary"))
  (save-script location *plot-average-saturation-script* "plot-average-saturation")
  (display-plot (namestring (make-pathname :name "summary" :type "png"
                                           :defaults location)) "Saturation")
  (display-plot (namestring (make-pathname :name "average-saturation" :type "png"
                                           :defaults location)) "Average Saturation"))

(defun display-plot (filename title)
  (declare (optimize (debug 3)))
  (let* ((dialog (q+:make-qdialog))
         (image (q+:make-qlabel))
         (layout (q+:make-qgridlayout dialog)))
    (q+:set-pixmap image (q+:make-qpixmap filename))
    (q+:add-widget layout image 0 0 1 1)
    (q+:set-window-title dialog title)
    (q+:set-modal dialog nil)
    (q+:show dialog)))

(defun make-save-button ()
  (parameters-interface::make-button-in-context
   "Save results in..."
   (lambda (model-show)
     (lambda (self)
       (declare (ignore self))
       (let ((choose-dir (q+:make-qfiledialog model-show
                                              "Save to location"
                                              (namestring (uiop/os:getcwd)))))
         (q+:set-file-mode choose-dir (q+:QFileDialog.directory))
         (q+:set-options choose-dir (q+:QFileDialog.show-dirs-only))
         (when (exec-dialog-p choose-dir)
           (let* ((location (uiop/pathname:ensure-directory-pathname
                             (pathname (first (q+:selected-files choose-dir)))))
                  (simulation (model-show-object model-show))
                  (darcy (darcy-simulation-model simulation))
                  (results (darcy-simulation-results simulation)))
             (flet ((c-transformer (effsat index)
                      (conductivity-at darcy effsat index))
                    (psi-transformer (effsat index)
                      (pressure-at darcy effsat index)))
               (ensure-directories-exist location)
               (save-data-csv location "saturation-table" results)
               (save-data-csv location "saturation-plot-table" results
                              :time-row-format "t = ~,1F h")
               (save-data-csv location "conductivity-table" results
                              :transformer #'c-transformer)
               (save-data-csv location "conductivity-plot-table" results
                              :transformer #'c-transformer
                              :time-row-format "t = ~,1F h")
               (save-data-csv location "pressure-table" results
                              :transformer #'psi-transformer)
               (save-data-csv location "pressure-plot-table" results
                              :transformer #'psi-transformer
                              :time-row-format "t = ~,1F h"))
             (with-open-file (out
                              (make-pathname :name "average-saturation" :type "csv"
                                             :defaults location)
                              :direction :output
                              :if-exists :supersede)
               (output-average-saturation-csv results out))
             (with-open-file (out
                              (make-pathname :name "model" :type "txt"
                                             :defaults location)
                               :direction :output
                               :if-exists :supersede)
               (format out "~A" darcy))
             (save-plot-scripts simulation location))))))))

(defun make-run-model-button ()
  "Produces the function of MODEL-SHOW that makes Run model button.
The click on the button runs the simulation.
This form is suitable to be used as an entry in the list for
:OBJECT-OPERATION-WIDGETS initarg of MODEL-SHOW"
  (parameters-interface::make-button-in-context
   "Run model"
   (lambda (model-show)
     (lambda (self)
       (declare (ignore self))
       ;; (break "Break ~A" model-show)
       (with-slots ((object parameters-interface::object)) model-show
         (format t "~&Sarting simulation~%")
         (simulate object)
         (q+:show (make-instance 'results-table :simulation object)))))))

(defun run-simulation ()
  (show-model (make-default-darcy-simulation)
              *name-substitutions*
              (list
               (make-load-configuration-button)
               (make-instantiate-button)
               (make-run-model-button)
               (make-save-button))))
