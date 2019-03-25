(ns example
  (:require [free-monad.core :refer :all]
            [clojure.test :refer :all]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as matchers]
            [cats.core :as m]))

(def is-invalid? #(= (:cpf/status %) :cpf.status/invalid))

(defn filter-invalid [cpfs]
  (filter is-invalid? cpfs))

(defn open-flow [flows] (some #(when (= (:flow/status %) :flow.status/open)
                                %) flows))

(defn new-flow [customer] {:flow/customer-id (:customer/id customer)
                           :flow/status     :flow.status/open})

(def close #(assoc % :flow/status :flow.status/close))

(def first-contact? #(empty? %))

(def send-reminder? #(= (count %) 1))

(def cancel-account? #(= (count %) 2))

(def close-flow? cancel-account?)

(def first-contact
  {:subject "Regularize seu CPF"
   :message "msg"})

(def reminder
  {:subject "Reminder: Regularize seu CPF"
   :message "msg"})

(defeffect customer-by-cpf [cpf])
(defeffect notify [flow msg])
(defeffect notifications-by-flow [flow])
(defeffect transact-flow [flow])
(defeffect flows-by-customer [customer])
(defeffect bureau-check [flow])
(defeffect cancel-account [flow])
(defeffect query-flows [query])

(defprogram process-flow [flow]
  [is-invalid? (bureau-check! flow)
   nots        (notifications-by-flow! flow)
   _           (if is-invalid?
                 (cond
                   (first-contact? nots)  (notify! flow first-contact)
                   (send-reminder? nots)  (notify! flow reminder)
                   (cancel-account? nots) (cancel-account! flow)
                   (close-flow? flow)     (transact-flow! (close flow)))
                 (transact-flow! (close flow)))]
  (m/return (close flow)))

(defprogram process-customer [customer]
  [flows (flows-by-customer! customer)
   flow  (if-let [flow (open-flow flows)]
           (process-flow flow)
           (transact-flow! (flow customer)))
   _     (process-flow flow)])

(defprogram process-flows []
  [flows (query-flows! {:flow/status :flow.status/open})
   _     (mapv process-flow flows)])

(defprogram process-cpfs [cpfs]
  [invalid-cpfs (-> cpfs filter-invalid pure)
   customers    (mapv customer-by-cpf! invalid-cpfs)
   _            (mapv process-customer customers)])

(defimpl test-impl [db]
  (customer-by-cpf
   [_ cpf]
   (:customer-by-cpf db))

  (notifications-by-flow
   [_ flow]
   (:notifications-by-flow db))

  (transact-flow
   [_ flow]
   flow)

  (flows-by-customer
   [_ customer]
   (:flows-by-customer db))

  (bureau-check
   [_ flow]
   (:bureau-check db))

  (notify [_ flow msg] nil))


(deftest test-invalid-without-open-flow
  (let [cpfs           [{:cpf/number "1" :cpf/status :cpf.status/invalid}
                        {:cpf/number "2" :cpf/status :cpf.status/valid}]
        customer       {:customer/id "1" :customer/cpf "1"}
        flow           (new-flow customer)
        closed-flow    (assoc (flow customer)
                              :flow/status :flow.status/closed)
        db             {:customer-by-cpf   customer
                        :notifications     []
                        :flows-by-customer [closed-flow]
                        :bureau-check      true}
        pgr            (process-cpfs cpfs)
        [effects impl] (logged-impl (test-impl db))]

    (run-program pgr impl)

    (is (match? [(customer-by-cpf! (nth cpfs 0))
                 (flows-by-customer! customer)
                 (transact-flow! flow)
                 (bureau-check! flow)
                 (notifications-by-flow! flow)
                 (notify! flow first-contact)]
                @effects))))

;; (deftest test-invalid-with-open-flow
;;   (let [cpfs           [{:cpf/number "1" :cpf/status :cpf.status/invalid}
;;                         {:cpf/number "2" :cpf/status :cpf.status/valid}]
;;         customer       {:customer/id "1" :customer/cpf "1"}
;;         flow           (flow customer)
;;         closed-flow    (assoc (flow customer)
;;                               :flow/status :flow.status/closed)
;;         db             {:customer          customer
;;                         :notifications     []
;;                         :flows-by-customer [closed-flow]}
;;         pgr            (process-cpfs cpfs)
;;         [effects impl] (logged-impl (test-impl db))]

;;       (run-program pgr impl)

;;       (is (match? [(customer-by-cpf! (nth cpfs 0))]
;;                   (customer-by-cpf! (nth cpfs 1))

;;                   (flows-by-customer! customer)
;;                   (transact-flow! flow)
;;                   (notifications-by-flow! flow)
;;                   (notify! flow first-contact)))))



(run-tests 'example)
