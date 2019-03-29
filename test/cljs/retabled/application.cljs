(ns retabled.application
  "The application with which users fill out the form to make or edit an application"
  (:require [reagent.core :as r]
            [reagent.session :as session]
            [retabled.shared-test :as shared :refer [cx]]
            [accountant.core]
            [retabled.routes :as rt]
            [retabled.core :as rfc]))

(defn validate-and-submit "Validate the form and submit"
  [form-dom-id]
  (let [form (.getElementById js/document form-dom-id)
        update-id (session/get :application)]
    (-> form .-classList (.add "was-validated"))
    (if (.checkValidity form)
      (js/alert "You have errors in your form. Please correct them before submitting."))))

(def jumbotext
  [:div.info
   [:p "After approval by the appropriate college, operational elements of the program being proposed will be evaluated by Kennedy Center staff, while its academic quality and effectiveness are reviewed by the Academic Oversight Committee. This committee places special emphasis on clarity of learning outcomes and demonstration of appropriate purpose and rigor. Programs will be listed in the catalog under the sponsoring unit, and learning outcomes for approved programs should be placed on the BYU learning outcomes site. The sponsoring academic unit is responsible for program outcomes; however, the Oversight Committee will periodically review learning effectiveness in international programs."]
   [:p "Guidelines for operational aspects of international study programs may be found on our Faculty Resources page at " [:a {:href "http://kennedy.byu.edu/retabled/faculty/index.php"} "http://kennedy.byu.edu/retabled/faculty/index.php"]
    ", listed as “Principles to Guide International Study Programs”. Programs must adhere to the BYU International Travel Policy found at " [:a {:href "travelsmart.byu.edu"} "travelsmart.byu.edu."]]])

(def my-atom (r/atom {:mything "hello"}))

(def test-form [:mytext {:type :text
                         :label "Text line"}
                :mytextarea {:type :textarea
                             :label "Text Area"}
                :mymultitable  {:label "My Multitable"
                                :id :mymulti
                                :required? true
                                :type :multi-table
                                :min-rows 3
                                :subtext "Indicate any expenses involved in carryout out your research, including a reason for each expense."
                                :value-path [:my-multitable]
                                ;:sum-field :amount
                                :columns [{:key :item
                                           :title "Item"}
                                          {:key :amount
                                           :title "Amount"
                                           :input-type "number"}
                                          {:key :purpose
                                           :title "Purpose"
                                           :input-type "textarea"}]}
                :mytoggle {:type :togglebox
                           :label "My togglebox"
                           :content [:test {:type :text :label "My toggled "}]}
                :mycheckbox {:type :checkbox :label "Check"}])

(defn generate-form []
  (let [form-id "needs-validation"
        validate (fn [e] (let [s (shared/get-value-from-change e)
                               element (.. e -target)]
                           (if (even? (count s))
                             (.setCustomValidity element "")
                             (.setCustomValidity element "You're odd!"))))]
    [:div.submission-form 
     [:form.form-control {:id form-id}
      (into [:div.form-contents
             #_[:a.btn.btn-success {:on-click #(.reportValidity (.getElementById js/document form-id))}
              "Validate"]]
                                        ;(rfc/render-application test-form my-atom)
            (rfc/render-application test-form {:READ (partial get-in @my-atom)
                                               :UPDATE (partial swap! my-atom update-in)})
            )]]))

(defn app-page []
  (shared/page-template {:jumbo-title "Reformation Application"
                         :contents [:div.mycontent
                                    [:h1 "My Form"]
                                    [generate-form]
                                    ]}))
