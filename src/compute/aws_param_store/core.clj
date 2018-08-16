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
(s/def ::path (s/or :string string?
                    :vector (s/coll-of any? :kind vector? :min-count 1)))

(defn client
  [opts]
  (impl/client opts))

(s/def ::profile string?)
(s/def ::region string?)
(s/fdef client
        :args (s/cat :opts (s/keys :opt-un [::profile ::region]))
        :ret ::client)

(defn path*
  [path]
  (let [p (cond
            (string? path) path
            (vector? path) (->> path
                                (filter identity)
                                (map (fn [x]
                                       (if (keyword? x)
                                         (subs (str x) 1)
                                         x)))
                                (str/join "/")))]
    ;; ensure path starts with / for consistency
    (if (str/starts-with? p "/")
      p
      (str "/" p))))

(defn path
  [& parts]
  (path* (vec parts)))

(defn parse-path
  [path]
  (into []
        (filter (complement str/blank?))
        (str/split path #"/")))

(s/fdef parse-path
        :args (s/cat :path-string string?)
        :ret ::path)

(defn parse-value
  [v]
  (try
    (edn/read-string v)
    (catch Exception _ v)))

;; API

(defn put-parameter!
  "Puts `value` at `path` into the Parameter Store."
  ([client path value] (put-parameter! client path value nil))
  ([client path value {:keys [encrypt?]}]
   (let [edn-val (pr-str value)]
     (assert (<= (count edn-val) 4096) "Parameter value must be less than 4096 characters")
     (impl/put-parameter client
                         (if encrypt?
                           "SecureString"
                           "String")
                         (path* path)
                         edn-val
                         {:overwrite? true}))))

(s/fdef put-parameter!
        :args (s/cat :client ::client
                     :path ::path
                     :value any?
                     :opts (s/? (s/nilable map?))))

(defn delete-parameter!
  "Deletes the parameter at `pathc`."
  [client path]
  (impl/delete-parameter client (path* path)))

(s/fdef delete-parameter!
        :args (s/cat :client ::client
                     :path ::path))

(defn get-parameter
  "Returns the parameter value at `path`, `nil` if the parameter does not exist."
  [client path]
  (let [{:keys [value]} (try
                          (impl/get-parameter client (path* path) {:decrypt? true})
                          (catch ParameterNotFoundException _ nil))]
    (parse-value value)))

(s/fdef get-parameter
        :args (s/cat :client ::client
                     :path ::path)
        :ret any?)

(defn get-parameters
  "Returns a map with all key/value pairs at the specified `path`."
  [client path]
  (let [params (impl/get-parameters-by-path client (path* path) {:decrypt? true})]
    (reduce (fn [params {:keys [name value]}]
              (assoc params name (parse-value value)))
            {} params)))

(s/fdef get-parameters
        :args (s/cat :client ::client
                     :path ::path)
        :ret map?)