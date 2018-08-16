(ns compute.aws-param-store.core
  "An opinionated wrapper around AWS parameter store."
  (:require
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [clojure.edn :as edn]
    [compute.aws-param-store.impl :as impl])
  (:import (com.amazonaws.services.simplesystemsmanagement.model ParameterNotFoundException)))

;; HELPERS

(s/def ::client impl/client?)
(s/def ::path-vec (s/coll-of keyword? :kind vector? :min-count 1))
(s/def ::path string?)

(defn client
  [opts]
  (impl/client opts))

(s/def ::profile string?)
(s/def ::region string?)
(s/fdef client
        :args (s/cat :opts (s/keys :opt-un [::profile ::region]))
        :ret ::client)

(defn stringify-path
  [path-vec]
  (->> path-vec
       (map (fn [kw]
              (if (qualified-keyword? kw)
                (str (namespace kw) ":" (name kw))
                (name kw))))
       (str/join "/")
       (str "/")))

(s/fdef stringify-path
        :args (s/cat :path ::path-vec)
        :ret ::path)

(defn- keyword-from-path-part
  [path-part]
  (let [kw-parts (str/split path-part #":" 2)]
    (apply keyword kw-parts)))

(defn parse-path
  [path]
  (let [path-parts (str/split path #"/")]
    (into []
          (comp
            (filter (complement str/blank?))
            (map keyword-from-path-part))
          path-parts)))

(s/fdef parse-path
        :args (s/cat :path ::path)
        :ret ::path-vec)

(defn parse-value
  [v]
  (try
    (edn/read-string v)
    (catch Exception _ v)))

;; API

(defn put-parameter!
  "Puts `value` at `path-vec` into the Parameter Store."
  ([client path-vec value] (put-parameter! client path-vec value nil))
  ([client path-vec value {:keys [encrypt?]}]
   (let [edn-val (pr-str value)]
     (assert (<= (count edn-val) 4096) "Parameter value must be less than 4096 characters")
     (impl/put-parameter client
                         (if encrypt?
                           "SecureString"
                           "String")
                         (stringify-path path-vec)
                         edn-val
                         {:overwrite? true}))))

(s/fdef put-parameter!
        :args (s/cat :client ::client
                     :path ::path-vec
                     :value any?
                     :opts (s/? (s/nilable map?))))

(defn delete-parameter!
  "Deletes the parameter at `path-vec`."
  [client path-vec]
  (impl/delete-parameter client (stringify-path path-vec)))

(s/fdef delete-parameter!
        :args (s/cat :client ::client
                     :path-vec ::path-vec))

(defn get-parameter
  "Returns the parameter value at `path-vec`, `nil` if the parameter does not exist."
  [client path-vec]
  (let [{:keys [value]} (try
                          (impl/get-parameter client (stringify-path path-vec) {:decrypt? true})
                          (catch ParameterNotFoundException _ nil))]
    (parse-value value)))

(s/fdef get-parameter
        :args (s/cat :client ::client
                     :path ::path-vec)
        :ret any?)

(defn get-parameters
  "Returns a map with all key/value pairs at the specified `path-vec`."
  [client path-vec]
  (let [params (impl/get-parameters-by-path client (stringify-path path-vec) {:decrypt? true})]
    (reduce (fn [params {:keys [name value]}]
              (assoc params (last (parse-path name)) (parse-value value)))
            {} params)))

(s/fdef get-parameters
        :args (s/cat :client ::client
                     :path ::path-vec)
        :ret map?)