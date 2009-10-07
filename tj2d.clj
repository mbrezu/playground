
;; This is a very simple Clojure program that uses Swing and 2d Java
;; graphics.  It is an exercise in remembering the Java Swing API and
;; programming in Clojure.

;; It is supposed to fill the window with randomly colored
;; squares. Moving the mouse over the window will repaint an area
;; around the mouse cursor with new randomly colored squares.

;; To use, run (make-frame)

(ns playground.tj2d
  (:import (java.awt Color Dimension Graphics Rectangle)
           (java.awt.event MouseMotionListener MouseEvent)
           (javax.swing JFrame JButton JPanel)))

(def *square-size* 8)

(defn draw-square [g x y]
  (let [color (Color. (int (rand-int 256))
                      (int (rand-int 256))
                      (int (rand-int 256)))]
    (.setColor #^Graphics g color)
    (.fillRect #^Graphics g
               (* x *square-size*)
               (* y *square-size*)
               (dec *square-size*)
               (dec *square-size*))))

(defn my-panel []
  (proxy [JPanel MouseMotionListener] []
    (paintComponent
     [g2d]
     (proxy-super paintComponent #^Graphics g2d)
     (let [maybe-clip (.getClipBounds #^Graphics g2d)
           clip (or maybe-clip (Rectangle. (.getSize this)))
           min-x (quot (.getX clip) *square-size*)
           max-x (quot (+ (.getX clip) (.getWidth clip)) *square-size*)
           min-y (quot (.getY clip) *square-size*)
           max-y (quot (+ (.getY clip) (.getHeight clip)) *square-size*)]
       (doseq [x (range min-x (inc max-x))
               y (range min-y (inc max-y))]
         (draw-square g2d x y))))
    (mouseMoved
     [e]
     (let [proper-x (* *square-size* (quot (.getX #^MouseEvent e) *square-size*))
           proper-y (* *square-size* (quot (.getY #^MouseEvent e) *square-size*))
           area-size 20]
       (.repaint #^JPanel this
                 (- proper-x (* (quot area-size 2) *square-size*))
                 (- proper-y (* (quot area-size 2) *square-size*))
                 (* area-size *square-size*)
                 (* area-size *square-size*))))))

(defn make-frame []
  (let [frame (doto (JFrame.)
                (.setSize (Dimension. 200 200)))
        panel (my-panel)
        panel-motion (doto panel
                       (.addMouseMotionListener panel))]
    (.. frame getContentPane (add panel-motion))
    (.show frame)))
