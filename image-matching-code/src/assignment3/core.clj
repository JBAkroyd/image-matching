(ns assignment3.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File])
  (:require [clojure.pprint :as pp])
  (:require [clojure.core.memoize :as memo]))

(def kirsch-filters
  "all kirsch filters"
  [
   [-1 0 1
    -2 0 2
    -1 0 1],
   [-2 -1 0
    -1 0 1
    0 1 2],
   [-1 -2 -1
    0 0 0
    1 2 1],
   [0 -1 -2
    1 0 -1
    2 1 0],
   [1 0 -1
    2 0 -2
    1 0 -1],
   [2 1 0
    1 0 -1
    0 -1 -2],
   [1 2 1
    0 0 0
    -1 -2 -1],
   [0 1 2
    -1 0 1
    -2 -1 0]
   ])

(defn new-image
  "Function to create a new image."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_BYTE_GRAY))

(defn read-image
  "read an image"
  [filename]
  (let [file (File. filename)]
    (ImageIO/read file)))

(defn save-image
  "saves an image"
  [image extension filename]
  (let [file (File. filename)]
    (ImageIO/write image extension file)))

(defn get-width
  "Function to get the width of an image."
  [image]
  (.getWidth image))

(defn get-height
  "Function to get the height of an image."
  [image]
  (.getHeight image))

(def get-value
  ;function to get the pixel value
  (memo/memo (fn
             [image x y]
             (let [rgb (.getRGB image x y)
                   red (bit-shift-right (bit-and rgb 0xFF0000) 16)]
               red))))

(def get-pixel-matrix
  ;this function get the pixels around the pixel and its self and puts it in a vector
  (memo/memo
    (fn
      [image x y]
      (let [tl (get-value image (- x 1) (- y 1))
            tm (get-value image x (- y 1))
            tr (get-value image (+ x 1) (- y 1))
            ml (get-value image (- x 1) y)
            mm (get-value image x y)
            mr (get-value image (+ x 1) y)
            bl (get-value image (- x 1) (+ y 1))
            bm (get-value image x (+ y 1))
            br (get-value image (+ x 1) (+ y 1))]
        (vector tl tm tr ml mm mr bl bm br)))))

(defn set-rgb
  "Function to set the RGB components of a pixel."
  [image x y [red green blue]]
  (let [rgb (+ (bit-shift-left red 16)
               (bit-shift-left blue 8)
               (bit-shift-left green 0))]
    (.setRGB image x y rgb)))

(defn set-grey
  "Function to set the grey value of a pixel."
  [image x y grey]
  (set-rgb image x y [grey grey grey]))

(defn clamp
  "clamps between two values"
  [value]
  ;if the value is less than 0 make it zero, if it is above 255 make it 255 else return its value
  (if (< value 0)
    0
    (if (> value 255)
      255
      value)))

(defn kirsch-val
  "gets the kirsch value of a pixel"
  [image x y i]
  ;gets the value of the pixel after the kirsch filter has been used
  (let [filter (kirsch-filters i)]
    (reduce + (map * (get-pixel-matrix image x y) filter))))

(defn kirsch
  "filter image with kirsch filter"
  [filename i]
  (let [orig-image (read-image filename)
        orig-image-h (get-height orig-image)
        orig-image-w (get-width orig-image)
        filtered-image (new-image orig-image-w orig-image-h)
        filename-out (str filename "-kirsch" i)]
    (dorun (pmap (fn [[x y]]
                   (set-grey filtered-image x y (clamp (+ 127 (kirsch-val orig-image x y i))))) (for [x (range 1 (- orig-image-w 1)) y (range 1 (- orig-image-h 1))] [x y])))
    (save-image filtered-image "jpg" filename-out)))

(defn normalise-hist
  [histogram total]
  ;this normalizes a histogram by taking the histogram and the number to divide it by
  (let [h histogram]
    (pmap (fn [i]
            (float (/ (get h i) total))) (range 0 8))))

(defn edge-magnitude-hist
  "filter image with kirsch filter and stores the maximum value into the histogram"
  [filename]
  (let [orig-image (read-image filename)
        orig-image-h (get-height orig-image)
        orig-image-w (get-width orig-image)
        histogram [0 0 0 0 0 0 0 0]
        ]
    (into []
          (normalise-hist
            (into []
                  (apply pmap +
                         (pmap (fn [[x y]]
                                 (let [h0 (kirsch-val orig-image x y 0)
                                       h1 (kirsch-val orig-image x y 1)
                                       h2 (kirsch-val orig-image x y 2)
                                       h3 (kirsch-val orig-image x y 3)
                                       h4 (kirsch-val orig-image x y 4)
                                       h5 (kirsch-val orig-image x y 5)
                                       h6 (kirsch-val orig-image x y 6)
                                       h7 (kirsch-val orig-image x y 7)
                                       max-h (clamp (max h0 h1 h2 h3 h4 h5 h6 h7))
                                       hist-pos (int (/ max-h 32))
                                       ]
                                   (update histogram hist-pos inc))) (for [x (range 1 (- orig-image-w 1)) y (range 1 (- orig-image-h 1))] [x y])))) (* (- orig-image-w 2) (- orig-image-h 2))))))

(defn edge-direction-hist
  "filter image with kirsch filter and stores what filter was used into the histogram"
  [filename]
  (let [orig-image (read-image filename)
        orig-image-h (get-height orig-image)
        orig-image-w (get-width orig-image)
        histogram [0 0 0 0 0 0 0 0]
        ]
    (into []
          (normalise-hist
            (into []
                  (apply pmap +
                         (pmap (fn [[x y]]
                                 (let [h0 (kirsch-val orig-image x y 0)
                                       h1 (kirsch-val orig-image x y 1)
                                       h2 (kirsch-val orig-image x y 2)
                                       h3 (kirsch-val orig-image x y 3)
                                       h4 (kirsch-val orig-image x y 4)
                                       h5 (kirsch-val orig-image x y 5)
                                       h6 (kirsch-val orig-image x y 6)
                                       h7 (kirsch-val orig-image x y 7)
                                       hvector [h0 h1 h2 h3 h4 h5 h6 h7]
                                       hist-pos (first (apply max-key second (map-indexed vector hvector)))
                                       ]
                                   (update histogram hist-pos inc))) (for [x (range 1 (- orig-image-w 1)) y (range 1 (- orig-image-h 1))] [x y])))) (* (- orig-image-w 2) (- orig-image-h 2))))))

(defn intensity-hist
  "gets the grey value of an image and puts it into a bin in the histogram"
  [filename]
  (let [orig-image (read-image filename)
        orig-image-h (get-height orig-image)
        orig-image-w (get-width orig-image)
        histogram [0 0 0 0 0 0 0 0]
        ]
    (into []
          (normalise-hist
            (into []
                  (apply pmap +
                         (pmap (fn [[x y]]
                                 (let [value (get-value orig-image x y)
                                       hist-pos (int (/ value 32))
                                       ]
                                   (update histogram hist-pos inc))) (for [x (range orig-image-w) y (range orig-image-h)] [x y])))) (* orig-image-w orig-image-h)))))

(def image-descriptor
  ;concats the histograms together to make an image descriptor
  (memo/memo (fn
             [file]
             (let [emh (normalise-hist (edge-magnitude-hist file) 3)
                   edh (normalise-hist (edge-direction-hist file) 3)
                   ih (normalise-hist (intensity-hist file) 3)
                   allh (into [] (concat emh edh ih))]
               allh))))

(def image-descriptor2
  ;concats the histograms together to make an image descriptor
  (memo/memo (fn
               [file]
               (let [emh (normalise-hist (edge-magnitude-hist file) 2)
                     edh (normalise-hist (edge-direction-hist file) 2)
                     allh (into [] (concat emh edh))]
                 allh))))

(defn clear-cache
  []
  (memo/memo-clear! get-value)
  (memo/memo-clear! get-pixel-matrix)
  (memo/memo-clear! image-descriptor))


(defn image-similarity-mag
  "checks if two images are similar by adding the minimum value of every bin to a total"
  [file1 file2]
  (let [f1 (image-descriptor file1)
        f2 (image-descriptor file2)]
    (reduce + (pmap (fn [i]
                      (float (min (get f1 i) (get f2 i)))) (range 0 8)))))

(defn image-similarity-dir
  "checks if two images are similar by adding the minimum value of every bin to a total"
  [file1 file2]
  (let [f1 (image-descriptor file1)
        f2 (image-descriptor file2)]
    (reduce + (pmap (fn [i]
                      (float (min (get f1 i) (get f2 i)))) (range 8 16)))))

(defn image-similarity-int
  "checks if two images are similar by adding the minimum value of every bin to a total"
  [file1 file2]
  (let [f1 (image-descriptor file1)
        f2 (image-descriptor file2)]
    (reduce + (pmap (fn [i]
                      (float (min (get f1 i) (get f2 i)))) (range 16 24)))))

(defn image-similarity
  "checks if two images are similar by adding the minimum value of every bin to a total"
  [file1 file2]
  (let [f1 (image-descriptor file1)
        f2 (image-descriptor file2)]
    (reduce + (pmap (fn [i]
                      (float (min (get f1 i) (get f2 i)))) (range 0 24)))))

(defn image-similarity-no-int
  "checks if two images are similar by adding the minimum value of every bin to a total"
  [file1 file2]
  (let [f1 (image-descriptor2 file1)
        f2 (image-descriptor2 file2)]
    (reduce + (pmap (fn [i]
                      (float (min (get f1 i) (get f2 i)))) (range 0 16)))))

(defn get-all-files
  [image-type]
  (let []
    (into [] (pmap (fn [i]
                     (str "vehicle_images/" image-type i ".jpg")) (range 1 21)))))

(defn get-all-sim-mag
  [image-type]
  (let [files (get-all-files image-type)
        counter 0]
    (pp/pprint (into [] (map (fn [f]
                             (inc counter)
                             (if (mod counter 10)
                               clear-cache)
                             (into [] (map (fn [g]
                                             (inc counter)
                                             (if (mod counter 10)
                                               clear-cache)
                                             (image-similarity-mag g f)) files))) files)))))

(defn get-all-sim-dir
  [image-type]
  (let [files (get-all-files image-type)
        counter 0]
    (pp/pprint (into [] (map (fn [f]
                             (inc counter)
                             (if (mod counter 10)
                               clear-cache)
                             (into [] (map (fn [g]
                                             (inc counter)
                                             (if (mod counter 10)
                                               clear-cache)
                                             (image-similarity-dir g f)) files))) files)))))

(defn get-all-sim-int
  [image-type]
  (let [files (get-all-files image-type)
        counter 0]
    (pp/pprint (into [] (map (fn [f]
                             (inc counter)
                             (if (mod counter 10)
                               clear-cache)
                             (into [] (map (fn [g]
                                             (inc counter)
                                             (if (mod counter 10)
                                               clear-cache)
                                             (image-similarity-int g f)) files))) files)))))

(defn get-all-sim
  [image-type]
  (let [files (get-all-files image-type)
        counter 0]
    (pp/pprint (into [] (map (fn [f]
                             (inc counter)
                             (if (mod counter 10)
                               clear-cache)
                             (reduce + (map (fn [g]
                                    (inc counter)
                                    (if (mod counter 10)
                                      clear-cache)
                                    (image-similarity g f)) files))) files)))))

(defn get-all-sim-no-int
  [image-type]
  (let [files (get-all-files image-type)
        counter 0]
    (pp/pprint (into [] (map (fn [f]
                               (inc counter)
                               (if (mod counter 10)
                                 clear-cache)
                               (reduce + (map (fn [g]
                                                (inc counter)
                                                (if (mod counter 10)
                                                  clear-cache)
                                                (image-similarity-no-int g f)) files))) files)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (get-all-sim-no-int "car"))
