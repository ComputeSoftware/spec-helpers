(ns computesoftware.spec-helpers.malli-registry
  (:require
    [malli.core :as m]
    [malli.registry]
    [malli.util]))

(defonce ^:private *registry (atom (merge (m/default-schemas) (malli.util/schemas))))
(def ^:private -mutable-registry (malli.registry/mutable-registry *registry))

(defn register!
  [k schema]
  (swap! *registry assoc k schema))

(defn get-registry [] -mutable-registry)

(defn get-schema [k] (malli.registry/schema -mutable-registry k))

;; Set the default registry to the mutable registry
(malli.registry/set-default-registry! -mutable-registry)
