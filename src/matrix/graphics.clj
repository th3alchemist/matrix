(ns aIo.matrix.graphics)

(require 'aIo.matrix.core 'aIo.matrix.math)
(alias 'mCore 'aIo.matrix.core)
(alias 'mMath 'aIo.matrix.math)

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