(ns computesoftware.spec-helpers
  #?(:cljs (:require-macros [computesoftware.spec-helpers :refer [number-in string-in keys-eval sorted-map-of and- sdef finite+]]))
  (:refer-clojure :exclude [infinite?])
  (:require
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [clojure.set :as sets]
    [spec-tools.core :as stools]
    [spec-tools.parse :as parse]
    [computesoftware.spec-helpers.gen :as h.gen]
    [computesoftware.spec-helpers.malli-registry :as malli-registry])
  #?(:clj (:import (java.time Duration)
                   (java.time.temporal TemporalUnit)
                   (java.util Date))))

(defonce __*spec->meta (atom {}))

(s/def ::sdef-args
  (s/cat
    :docstring (s/? string?)
    :metamap (s/? some?)
    :spec some?))

#?(:clj
   (defn sdef-form
     [k argm]
     (when (s/invalid? argm)
       (throw (ex-info "Invalid call to sdef."
                {:explain (s/explain-data ::sdef-args argm)})))
     (let [{:keys [metamap
                   docstring
                   spec]} argm
           metamap (cond-> metamap
                     docstring (assoc :doc docstring))
           mm-sym (gensym "metamap")]
       `(let [~mm-sym ~metamap]
          (s/def ~k ~spec)
          ~(when metamap
             `(swap! __*spec->meta assoc ~k ~mm-sym))
          (when-let [schema# (:schema ~mm-sym)]
            (malli-registry/register! ~k schema#))
          nil))))

#?(:clj
   (defmacro sdef
     "Like s/def but supports setting a map of metadata."
     {:arglists '([name doc-string? metamap? spec])}
     [k & sdef-decl]
     (sdef-form k (s/conform ::sdef-args sdef-decl))))

(defn smeta
  "Returns the metamap for k or nil if it does not exist."
  [k]
  (get @__*spec->meta k))

(def int-min-value
  #?(:clj  Integer/MIN_VALUE
     :cljs js/Number.MIN_SAFE_INTEGER))

(def int-max-value
  #?(:clj  Integer/MAX_VALUE
     :cljs js/Number.MAX_SAFE_INTEGER))

(def long-min-value
  #?(:clj  Long/MIN_VALUE
     :cljs js/Number.MIN_SAFE_INTEGER))

(def long-max-value
  #?(:clj  Long/MAX_VALUE
     :cljs js/Number.MAX_SAFE_INTEGER))

(def double-max-value
  #?(:clj  Double/MAX_VALUE
     :cljs js/Number.MAX_SAFE_INTEGER))

(def double-min-value
  #?(:clj  (- double-max-value)
     :cljs js/Number.MIN_SAFE_INTEGER))

(defn next-up
  [x]
  #?(:clj  (Math/nextUp ^double x)
     :cljs (+ js/Number.MIN_VALUE x)))

(defn char-code-for
  "A number representing the UTF-16 code unit value of s. s must be a string
  of length 1."
  [s]
  #?(:clj (int (first s)) :cljs (.charCodeAt s 0)))

(def uppercase-letter-set
  (into #{} (map (comp str char)) (range (char-code-for "A") (char-code-for "Z"))))

(def lowercase-letter-set
  (into #{} (map str/lower-case) uppercase-letter-set))

(s/def ::upper-case-letter uppercase-letter-set)
(s/def ::lower-case-letter lowercase-letter-set)

#?(:clj
   (defmacro sorted-map-of
     [kpred vpred & opts]
     (let [sform `(s/map-of ~kpred ~vpred ~@opts)]
       `(s/with-gen
          ~sform
          #(gen/fmap (fn [m#] (into (sorted-map) m#)) (s/gen ~sform))))))

#?(:clj
   ;; clj only since cljs does not have an alias function.
   (defn alias2
     [alias-sym ns-sym]
     (create-ns ns-sym)
     (alias alias-sym ns-sym)))

(defn safe-explain-data
  "Try to remove the possibility of large values existing in the explain data
  message."
  [spec x]
  (update (s/explain-data spec x)
    ::s/problems (fn [problems]
                   (map #(dissoc % :val) problems))))

(let [f #'stools/map-spec-keys]
  (defn map-spec-keys
    [spec]
    (sets/rename-keys (f spec) {::parse/keys     :keys
                                ::parse/keys-req :keys-req
                                ::parse/keys-opt :keys-opt})))

(defn not-blank?
  [x]
  (not (str/blank? x)))

(defmacro and-
  "Same as s/and except all predicates take the 'raw' value instead of the conformed
  value."
  [& pred-forms]
  (let [pred-forms (map (fn [pred-form]
                          `(s/nonconforming ~pred-form)) pred-forms)]
    `(s/and ~@pred-forms)))

(defn string-in-gen
  [min max]
  (h.gen/let [size (s/gen (s/int-in (or min 0) (inc (or max 100))))
              chars (gen/vector (gen/char-alphanumeric) size)]
    (apply str chars)))

(defmacro string-in
  [& {:keys [min max]}]
  (let [form `(s/and
                string?
                ~@(when max `[(fn [~'%] (<= (count ~'%) ~max))])
                ~@(when min `[(fn [~'%] (<= ~min (count ~'%)))]))
        opts (cond-> {}
               min (assoc :min min)
               max (assoc :max max))]
    `(stools/spec
       {:spec   ~form
        :form   '~form
        :gen    #(string-in-gen ~min ~max)
        :schema ~(cond-> [:string]
                   (seq opts)
                   (conj opts))})))

(s/def ::not-blank-string (string-in :min 1))

;; Datomic Cloud has a max string length of 4096. We should really avoid transacting
;; that size of string due to memory pressure. Large strings should get stored
;; elsewhere.
;; https://docs.datomic.com/cloud/schema/schema-reference.html#notes-on-value-types
;; https://forum.datomic.com/t/4096-character-string-limit/1579
(s/def ::datomic-string (string-in :min 0 :max 4096))
(s/def ::datomic-string-not-blank-string (string-in :min 1 :max 4096))

(s/def ::email
  (s/spec
    (s/and string? #(re-matches #".+@.+\..+" %))
    :gen
    #(h.gen/let
       [name (h.gen/non-empty-string-alphanumeric)
        host (h.gen/non-empty-string-alphanumeric)
        tld (s/gen #{"com" "org" "net" "co"})]
       (str name "@" host "." tld))))

(defn infinite?
  "Returns true if x is infinite."
  [x]
  #?(:clj  (Double/isInfinite x)
     :cljs (not (js/isFinite x))))

(defn nan?
  "Returns true if x is NaN."
  [x]
  #?(:clj  (Double/isNaN x)
     :cljs (js/isNaN x)))

(defn number-in-gen
  [gen-argm]
  (gen/one-of
    [(gen/large-integer* (select-keys gen-argm [:min :max]))
     (gen/double (select-keys gen-argm [:min :max :NaN? :infinite?]))]))

#?(:clj
   (defmacro number-in
     "Returns a spec for an int or a double."
     [& {:keys [min max NaN? infinite?]
         :or   {NaN?      false
                infinite? false}}]
     (let [preds (cond-> `[number?]
                   (not infinite?) (conj `(fn [~'%] (not (infinite? ~'%))))
                   (not NaN?) (conj `(fn [~'%] (not (nan? ~'%))))
                   max (conj `(fn [~'%] (<= ~'% ~max)))
                   min (conj `(fn [~'%] (<= ~min ~'%))))
           form (if (= 1 (count preds))
                  (first preds)
                  (list* `s/and preds))
           opts (cond-> {}
                  min (assoc :min min)
                  max (assoc :max max)
                  NaN? (assoc :NaN? NaN? :gen/NaN? NaN?)
                  infinite? (assoc :infinite? infinite? :gen/infinite? infinite?))]
       `(s/nonconforming
          (stools/spec
            {:spec   ~form
             :form   '~form
             :gen    #(number-in-gen ~opts)
             :schema ~(cond-> [:double] (seq opts) (conj opts))})))))

(s/def ::finite (number-in :infinite? false :NaN? false))

#?(:clj
   (defmacro finite+
     [& {:keys [max] :as opts}]
     `(number-in :min (next-up 0.0) ~@(mapcat identity (assoc opts :infinite? false :NaN? false)))))

(s/def ::finite+ (finite+))

#?(:clj
   (defmacro finite-non-
     [& {:keys [max] :as opts}]
     `(number-in :min 0.0 ~@(mapcat identity opts))))

(s/def ::finite-non-
  (s/with-gen
    (number-in :min 0 :infinite? false :NaN? false)
    #(s/gen
       (s/or :int (s/int-in 0 long-max-value)
         :double (s/double-in :min 0.0 :NaN? false :infinite? false)))))

(s/def ::timestamp
  (s/with-gen int?
    #(s/gen (s/int-in (inst-ms #inst"1900") (inst-ms #inst"3000")))))

(s/def ::sane-inst
  (s/with-gen
    inst?
    #(gen/fmap (fn [epoch] #?(:clj (Date. ^long epoch) :cljs (js/Date. epoch)))
       (s/gen ::timestamp))))

#?(:clj
   (defn duration-generator
     "Generator for Java Duration objects."
     [{:keys [min-duration max-duration]
       :or   {min-duration Duration/ZERO
              max-duration (Duration/ofDays 300)}}]

     (h.gen/let [[dur-ctor convert-fn]
                 (s/gen #{[(fn [x] (Duration/ofNanos x))
                           (memfn toNanos)]
                          [(fn [x] (Duration/ofMillis x))
                           (memfn toMillis)]
                          [(fn [x] (Duration/ofMinutes x))
                           (memfn toMinutes)]
                          [(fn [x] (Duration/ofHours x))
                           (memfn toHours)]
                          [(fn [x] (Duration/ofDays x))
                           (memfn toDays)]})
                 dur-amount (s/gen (s/int-in (convert-fn min-duration) (convert-fn max-duration)))]
       (dur-ctor dur-amount))))

#?(:clj
   (defn duration-within-bounds?
     [duration min max]
     (let [nanos (.toNanos duration)]
       (and
         (if min
           (<= (.toNanos min) nanos)
           true)
         (if max
           (< nanos (.toNanos max))
           true)))))

#?(:clj
   (defmacro duration-in
     [& {:keys [min max]}]
     `(s/with-gen
        (s/and #(instance? Duration %)
          #(duration-within-bounds? % ~min ~max))
        #(duration-generator {~@(when min [:min-duration min])
                              ~@(when max [:max-duration max])}))))


#?(:clj
   (s/def ::duration
     (s/with-gen
       #(instance? Duration %)
       #(duration-generator {}))))

(defn date-generator
  [{:keys [min-date max-date]
    :or   {min-date #inst "1900"
           max-date #inst "3000"}}]
  (gen/fmap
    #?(:clj  #(Date. ^long %)
       :cljs #(js/Date. %))
    (s/gen
      (s/int-in
        (inst-ms min-date)
        (inst-ms max-date)))))

(s/def ::date
  (s/spec inst? :gen #(date-generator {})))

(s/def ::uuid-str
  (s/spec
    (s/conformer
      (fn [x]
        #?(:clj  (try
                   (java.util.UUID/fromString x)
                   (catch Exception _ ::s/invalid))
           ;; not exactly correct, but sufficient for now
           :cljs (uuid x)))
      str)
    :gen (fn [] (gen/fmap str (s/gen uuid?)))))

(s/def ::pos-int-str
  #?(:clj  (s/with-gen
             #(try
                (pos? (Integer/parseInt %))
                true
                (catch Exception _ false))
             #(h.gen/let [a-int (s/gen pos-int?)]
                (str a-int)))
     :cljs #(pos? (js/parseInt %))))

(defn exact-keys?
  [m kset]
  (= (set (keys m)) kset))

#?(:clj
   (defmacro keys-eval
     "Like s/keys but will eval the key args"
     [& {:keys [req req-un opt opt-un gen closed?]}]
     (let [eval-ks (fn [ks]
                     (when ks
                       (if (vector? ks)
                         ks
                         (eval ks))))
           req-ks (eval-ks req)
           req-un-ks (eval-ks req-un)
           opt-ks (eval-ks opt)
           opt-un-ks (eval-ks opt-un)
           all-kset (set (concat req-ks req-un-ks opt-ks opt-un-ks))
           keys-form `(s/keys
                        ~@(when req [:req req-ks])
                        ~@(when req-un [:req-un req-un-ks])
                        ~@(when opt [:opt opt-ks])
                        ~@(when opt-un [:opt-un opt-un-ks])
                        ~@(when gen [:gen gen]))]
       (if closed?
         `(s/and ~keys-form (fn [~'m] (exact-keys? ~'m ~all-kset)))
         keys-form))))

#?(:clj
   (defmacro closed-multi-spec
     [name dispatch-key dispatch->spec]
     (let [defmethods (map (fn [[dispatch spec-form]]
                             `(defmethod ~name ~dispatch
                                [~'_]
                                ~spec-form))
                        dispatch->spec)]
       `(do
          (defmulti ~name ~dispatch-key)
          ~@defmethods
          (s/multi-spec ~name ~dispatch-key)))))

(defn spec-form
  [keyword-or-form]
  (if (or (keyword? keyword-or-form)
        (s/spec? keyword-or-form))
    (s/form keyword-or-form)
    keyword-or-form))

(defn parse-spec-keys
  "Given a `spec`, returns a set of keys the spec has."
  [spec]
  (let [form (spec-form spec)
        [form-sym & form-args] form
        combine-composite-specs (fn [forms]
                                  (into #{} (mapcat parse-spec-keys) forms))]
    (case form-sym
      (clojure.spec.alpha/keys
        cljs.spec.alpha/keys)
      (->> (rest form-args)
        (take-nth 2)
        (flatten)
        (filter keyword?)
        (set))

      #?@(:clj
          [(clojure.spec.alpha/multi-spec
             cljs.spec.alpha/multi-spec)
           (let [multf @(resolve (first form-args))]
             (combine-composite-specs
               (map (fn [[_ multi-impl-f]]
                      (multi-impl-f nil))
                 (methods multf))))])

      (clojure.spec.alpha/merge
        cljs.spec.alpha/merge
        clojure.spec.alpha/and
        cljs.spec.alpha/and)
      (combine-composite-specs form-args)


      (clojure.spec.alpha/or
        cljs.spec.alpha/or)
      (combine-composite-specs (->> (rest form-args) (take-nth 2)))

      (throw (ex-info (str "Unsupported spec form " (pr-str form-sym) ".")
               {:form form})))))

;; TODO
#?(:clj
   (defn cleanup-check-result
     [result]
     (let [ret (:clojure.spec.test.check/ret result)
           sym (:sym result)
           base-data (select-keys result [:sym :spec])]
       (if (:pass? ret)
         result
         (let [shrunk-result (:shrunk ret)
               result-type (cond
                             ;; a generator exception can occur if a generator throws in its ctor function
                             ;; or if it throws when generating a value
                             (and (instance? Throwable (:result ret))
                               (nil? shrunk-result))
                             :generator-exception
                             (instance? Throwable (:result ret))
                             :function-exception
                             (and shrunk-result (not (:pass? shrunk-result)))
                             :shrunk-failed
                             :else :failed)]
           (case result-type
             :generator-exception
             (throw (ex-info "A generator threw an exception." base-data (:result ret)))
             :function-exception
             (throw (ex-info (str sym " threw an exception.")
                      base-data (:result ret)))
             (:shrunk-failed :failed)
             (-> shrunk-result
               (dissoc :result-data :smallest)
               (assoc :args (first (:smallest result))))))))))

#_(defn check
    ([sym-or-syms] (check sym-or-syms nil))
    ([sym-or-syms opts]
     (let [results (st/check sym-or-syms opts)]
       (map cleanup-check-result results))))


#_(defn add2
    [x y]
    ;(throw (ex-info "asd" {}))
    #_(+ x y) "")

#_(s/fdef add2
    :args (s/cat :x int? #_(s/with-gen int?
                             (h.gen/gen-let [_ (gen/return "asd")]
                               (throw (ex-info "throw in gen" {})))
                             #_#(throw (ex-info "gen throws" {})))
            :y int?)
    :ret int?)

(comment
  (:clojure.spec.test.check/ret (first (st/check `add2)))
  (check `add2))
