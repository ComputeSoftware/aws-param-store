(ns compute.aws-param-store.impl
  (:import (com.amazonaws.services.simplesystemsmanagement AWSSimpleSystemsManagementClientBuilder AWSSimpleSystemsManagementClient)
           (com.amazonaws.auth.profile ProfileCredentialsProvider)
           (com.amazonaws.services.simplesystemsmanagement.model PutParameterRequest DeleteParameterRequest GetParameterRequest Parameter GetParametersByPathRequest GetParametersByPathResult GetParameterResult)))

(defn client?
  [x]
  (instance? AWSSimpleSystemsManagementClient x))

(defn client
  [{:keys [profile region]}]
  (let [builder (let [builder (AWSSimpleSystemsManagementClientBuilder/standard)]
                  (cond-> builder
                          profile (.withCredentials (ProfileCredentialsProvider. profile))
                          region (.withRegion ^String region))
                  builder)]
    (.build builder)))

;; type is :string, :secure-string, or :string-list
(defn put-parameter
  [client type param-name param-value {:keys [description key-id allowed-pattern overwrite?]}]
  (.putParameter ^AWSSimpleSystemsManagementClient client
                 (cond-> (PutParameterRequest.)
                         true (.withName param-name)
                         true (.withValue param-value)
                         true (.withType ^String type)
                         description (.withDescription description)
                         overwrite? (.withOverwrite overwrite?)
                         key-id (.withKeyId key-id)
                         allowed-pattern (.withAllowedPattern allowed-pattern))))

(defn delete-parameter
  [client param-name]
  (.deleteParameter client (.withName (DeleteParameterRequest.) param-name)))

(defn map-from-parameter
  [^Parameter parameter]
  {:arn                (.getARN parameter)
   :last-modified-date (.getLastModifiedDate parameter)
   :name               (.getName parameter)
   :selector           (.getSelector parameter)
   :type               (.getType parameter)
   :value              (.getValue parameter)
   :version            (.getVersion parameter)})

(defn get-parameter
  [client param-name {:keys [decrypt?]}]
  (let [resp ^GetParameterResult (.getParameter client (cond-> (GetParameterRequest.)
                                                               true (.withName param-name)
                                                               decrypt? (.withWithDecryption decrypt?)))]
    (map-from-parameter (.getParameter resp))))

(defn get-parameters-by-path
  [client path {:keys [recursive? decrypt?]}]
  (let [resp ^GetParametersByPathResult (.getParametersByPath client (cond-> (GetParametersByPathRequest.)
                                                                             true (.withPath path)
                                                                             recursive? (.withRecursive recursive?)
                                                                             decrypt? (.withWithDecryption decrypt?)))]
    (into []
          (map map-from-parameter)
          (.getParameters resp))))

;; write tests