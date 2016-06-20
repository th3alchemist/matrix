(ns matrix.graphics)

(require 'matrix.core 'matrix.math)
(alias 'mCore 'matrix.core)
(alias 'mMath 'matrix.math)

(defn sheer-matrix [x]
  nil)

(defn scale-matrix [x]
  (mMath/scalar-mul (mCore/identity-matrix 2) x))

(defn rotation-matrix [x]
  nil)

(defn translation-matrix [x]
  nil)

(defn sheer [n M]
  nil)

(defn scale [pt x]
  (mMath/matrix-mul (scale-matrix x) pt))

(defn rotation [M n]
  nil)

(defn translation [M n]
  nil)