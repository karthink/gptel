;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-test-backends)
(require 'cl-lib)
(require 'map)

;; Read an API output stream and construct the response and tool calls

;;; General

(defvar gptel-test-tools
  (list
   (gptel--make-tool
    :function #'identity
    :name "read_url"
    :description "Fetch and read the contents of a URL"
    :args (list '(:name "url"
                  :description "The URL to read"
                  :type string))
    :category "web")
   (gptel--make-tool
    :function #'identity
    :name "get_weather"
    :description "Get the current weather in a given location"
    :args '((:name "location"
             :description "The latitude and longitude, in degrees.
South and West (resp) are negative."
             :type object
             :properties (:lat (:type number
                                :description "Latitude, [-90.0, 90.0]")
                          :lon (:type number
                                :description "Longitude, [-180.0, 180.0]"))
             :required ["lat" "lon"])
            (:name "unit"
             :description "The unit of temperature, either 'celsius' or 'fahrenheit'"
             :type string
             :enum ["celsius" "farenheit"]
             :optional t)))
   (gptel--make-tool
    :function #'identity
    :name "record_summary"
    :description "Record summary of an image using well-structured JSON."
    :confirm t
    :include t
    :category "testing"
    :args
    (list '(:name "key_colors"
            :description "Key colors in the image.  Limit to less than four."
            :type array
            :items (:type object
                    :properties (:r (:type number :description "red value [0.0, 1.0]")
                                 :g (:type number :description "green value [0.0, 1.0]")
                                 :b (:type number :description "blue value [0.0, 1.0]")
                                 :name (:type string :description: "Human-readable color name in snake_case, e.g. \"olive_green\" or \"turquoise\""))
                    :required ["r" "g" "b" "name"]))

          '(:name "description"
            :description "Image description.  One to two sentences max."
            :type string)

          '(:name "estimated_year"
            :description "Estimated year that the images was taken, if is it a photo. Only set this if the image appears to be non-fictional. Rough estimates are okay!"
            :type integer
               :optional t)))
   (gptel--make-tool
    :function #'identity
    :name "create_pull_request_review"
    :description "Create a review on a pull request"
    :async t
    :category "testing"
    :confirm nil
    :include nil
    :args
    (list
     '(:name "body" :description "Review comment text" :type "string")
     '(:name "comments" :description "Line-specific comments array of objects to place comments on pull request changes. Requires path and body. For line comments use line or position. For multi-line comments use start_line and line with optional side parameters."
       :type "array"
       :items (:additionalProperties :json-false
               :properties
               (:body
                (:description "comment body" :type "string")
                :line
                (:anyOf
                 [(:type "number")
                  (:type "null")]
                 :description "line number in the file to comment on. For multi-line comments, the end of the line range")
                :path
                (:description "path to the file" :type "string")
                :position
                (:anyOf
                 [(:type "number")
                  (:type "null")]
                 :description "position of the comment in the diff")
                :side
                (:anyOf
                 [(:type "string")
                  (:type "null")]
                 :description "The side of the diff on which the line resides. For multi-line comments, this is the side for the end of the line range. (LEFT or RIGHT)")
                :start_line
                (:anyOf
                 [(:type "number")
                  (:type "null")]
                 :description "The first line of the range to which the comment refers. Required for multi-line comments.")
                :start_side
                (:anyOf
                 [(:type "string")
                  (:type "null")]
                 :description "The side of the diff on which the start line resides for multi-line comments. (LEFT or RIGHT)"))
               :required ["path" "body" "position" "line" "side" "start_line" "start_side"]
               :type "object"))
     '(:name "commitId" :description "SHA of commit to review" :type "string")
     '(:name "event" :description "Review action to perform" :enum ["APPROVE" "REQUEST_CHANGES" "COMMENT"] :type "string")
     '(:name "owner" :description "Repository owner" :type "string")
     '(:name "pullNumber" :description "Pull request number" :type "number")))
   (gptel--make-tool
    :function #'identity
    :name "create_database"
    :description "Create a new MongoDB database with option to switch"
    :args '((:name "name"
             :description "Database name to create"
             :type string
             :minLength 1)
            (:name "switch"
             :description "Whether to switch to the new database after creation"
             :allOf [(:type string) (:type string :enum ["true" "false"])]
             :default "false")
            (:name "validateName"
             :description "Whether to validate database name"
             :allOf [(:type string) (:type string :enum ["true" "false"])]
             :default "true")))))

(defvar gptel-test-tools-openai
  [(:type "function" :function
    (:name "read_url" :description "Fetch and read the contents of a URL"
     :parameters
     (:type "object" :properties
      (:url (:description "The URL to read" :type "string")) :required ["url"]
      :additionalProperties :json-false)))
   (:type "function" :function
    (:name "get_weather"
     :description "Get the current weather in a given location"
     :parameters (:type "object"
                  :properties
                  (:location
                   (:description
                    "The latitude and longitude, in degrees.\nSouth and West (resp) are negative."
                    :type "object"
                    :properties
                    (:lat (:type "number" :description "Latitude, [-90.0, 90.0]")
                     :lon (:type "number" :description "Longitude, [-180.0, 180.0]"))
                    :required ["lat" "lon"])
                   :unit
                   (:description "The unit of temperature, either 'celsius' or 'fahrenheit'"
                    :type "string"
                    :enum ["celsius" "farenheit"]))
                  :required ["location"] :additionalProperties :json-false)))
   (:type "function" :function
    (:name "record_summary"
     :description "Record summary of an image using well-structured JSON."
     :parameters (:type "object" :properties
                  (:key_colors
                   (:description "Key colors in the image.  Limit to less than four."
                    :type "array"
                    :items (:type "object" :properties
                            (:r (:type "number" :description "red value [0.0, 1.0]")
                             :g (:type "number" :description "green value [0.0, 1.0]")
                             :b (:type "number" :description "blue value [0.0, 1.0]")
                             :name (:type "string" :description:
                                    "Human-readable color name in snake_case, e.g. \"olive_green\" or \"turquoise\""))
                            :required ["r" "g" "b" "name"]))
                   :description
                   (:description "Image description.  One to two sentences max."
                    :type "string")
                   :estimated_year
                   (:description
                    "Estimated year that the images was taken, if is it a photo. Only set this if the image appears to be non-fictional. Rough estimates are okay!"
                    :type "integer"))
                  :required ["key_colors" "description"]
                  :additionalProperties :json-false)))
   (:type "function" :function
    (:name "create_pull_request_review"
     :description "Create a review on a pull request"
     :parameters (:type "object" :properties
                  (:body
                   (:description "Review comment text" :type "string")
                   :comments
                   (:description "Line-specific comments array of objects to place comments on pull request changes. Requires path and body. For line comments use line or position. For multi-line comments use start_line and line with optional side parameters."
                    :type "array"
                    :items (:additionalProperties :json-false
                            :properties (:body
                                         (:description "comment body" :type "string")
                                         :line
                                         (:anyOf [(:type "number") (:type "null")]
                                          :description "line number in the file to comment on. For multi-line comments, the end of the line range")
                                         :path
                                         (:description "path to the file" :type "string")
                                         :position
                                         (:anyOf [(:type "number") (:type "null")]
                                          :description "position of the comment in the diff")
                                         :side
                                         (:anyOf [(:type "string") (:type "null")]
                                          :description "The side of the diff on which the line resides. For multi-line comments, this is the side for the end of the line range. (LEFT or RIGHT)")
                                         :start_line
                                         (:anyOf [(:type "number") (:type "null")]
                                          :description "The first line of the range to which the comment refers. Required for multi-line comments.")
                                         :start_side
                                         (:anyOf [(:type "string") (:type "null")]
                                          :description "The side of the diff on which the start line resides for multi-line comments. (LEFT or RIGHT)"))
                            :required ["path" "body" "position" "line" "side" "start_line" "start_side"]
                            :type "object"))
                   :commitId
                   (:description "SHA of commit to review" :type "string")
                   :event
                   (:description "Review action to perform" :enum ["APPROVE" "REQUEST_CHANGES" "COMMENT"] :type "string")
                   :owner
                   (:description "Repository owner" :type "string")
                   :pullNumber
                   (:description "Pull request number" :type "number")
                   :repo
                   (:description "Repository name" :type "string"))
                  :required ["body" "comments" "commitId" "event" "owner" "pullNumber" "repo"]
                  :additionalProperties :json-false)))
   (:type "function" :function
    (:name "create_database"
     :description "Create a new MongoDB database with option to switch"
     :parameters (:type "object" :properties
                  (:name
                   (:description "Database name to create"
                    :type "string"
                    :minLength 1)
                   :switch
                   (:description "Whether to switch to the new database after creation"
                    :allOf [(:type "string") (:type "string" :enum ["true" "false"])]
                    :default "false")
                   :validateName
                   (:description "Whether to validate database name"
                    :allOf [(:type "string") (:type "string" :enum ["true" "false"])]
                    :default "true"))
                  :required ["name" "switch" "validateName"]
                  :additionalProperties :json-false)))])

(defvar gptel-test-tools-anthropic
  [(:name "read_url" :description "Fetch and read the contents of a URL"
    :input_schema
    (:type "object" :properties
     (:url (:description "The URL to read" :type "string")) :required ["url"]))
   (:name "get_weather" :description "Get the current weather in a given location"
    :input_schema
    (:type "object" :properties
     (:location
      (:description
       "The latitude and longitude, in degrees.\nSouth and West (resp) are negative."
       :type "object" :properties
       (:lat (:type "number" :description "Latitude, [-90.0, 90.0]") :lon
        (:type "number" :description "Longitude, [-180.0, 180.0]"))
       :required ["lat" "lon"])
      :unit
      (:description "The unit of temperature, either 'celsius' or 'fahrenheit'"
       :type "string" :enum ["celsius" "farenheit"]))
     :required ["location"]))
   (:name "record_summary" :description
    "Record summary of an image using well-structured JSON." :input_schema
    (:type "object" :properties
     (:key_colors
      (:description "Key colors in the image.  Limit to less than four." :type
       "array" :items
       (:type "object" :properties
        (:r (:type "number" :description "red value [0.0, 1.0]") :g
         (:type "number" :description "green value [0.0, 1.0]") :b
         (:type "number" :description "blue value [0.0, 1.0]") :name
         (:type "string" :description:
          "Human-readable color name in snake_case, e.g. \"olive_green\" or \"turquoise\""))
        :required ["r" "g" "b" "name"]))
      :description
      (:description "Image description.  One to two sentences max." :type "string")
      :estimated_year
      (:description
       "Estimated year that the images was taken, if is it a photo. Only set this if the image appears to be non-fictional. Rough estimates are okay!"
       :type "integer"))
     :required ["key_colors" "description"]))
   (:name "create_pull_request_review" :description "Create a review on a pull request" :input_schema
    (:type "object" :properties
     (:body
      (:description "Review comment text" :type "string")
      :comments
      (:description "Line-specific comments array of objects to place comments on pull request changes. Requires path and body. For line comments use line or position. For multi-line comments use start_line and line with optional side parameters." :type "array" :items
       (:additionalProperties :json-false :properties
        (:body
         (:description "comment body" :type "string")
         :line
         (:anyOf
          [(:type "number")
           (:type "null")]
          :description "line number in the file to comment on. For multi-line comments, the end of the line range")
         :path
         (:description "path to the file" :type "string")
         :position
         (:anyOf
          [(:type "number")
           (:type "null")]
          :description "position of the comment in the diff")
         :side
         (:anyOf
          [(:type "string")
           (:type "null")]
          :description "The side of the diff on which the line resides. For multi-line comments, this is the side for the end of the line range. (LEFT or RIGHT)")
         :start_line
         (:anyOf
          [(:type "number")
           (:type "null")]
          :description "The first line of the range to which the comment refers. Required for multi-line comments.")
         :start_side
         (:anyOf
          [(:type "string")
           (:type "null")]
          :description "The side of the diff on which the start line resides for multi-line comments. (LEFT or RIGHT)"))
        :required
        ["path" "body" "position" "line" "side" "start_line" "start_side"]
        :type "object"))
      :commitId
      (:description "SHA of commit to review" :type "string")
      :event
      (:description "Review action to perform" :enum
       ["APPROVE" "REQUEST_CHANGES" "COMMENT"]
       :type "string")
      :owner
      (:description "Repository owner" :type "string")
      :pullNumber
      (:description "Pull request number" :type "number")
      :repo
      (:description "Repository name" :type "string"))
     :required
     ["body" "comments" "commitId" "event" "owner" "pullNumber" "repo"]))
   (:name "create_database" :description
    "Create a new MongoDB database with option to switch" :input_schema
    (:type "object" :properties
     (:name (:description "Database name to create" :type "string" :minLength 1)
      :switch
      (:description "Whether to switch to the new database after creation" :allOf
       [(:type "string") (:type "string" :enum ["true" "false"])] :default "false")
      :validateName
      (:description "Whether to validate database name" :allOf
       [(:type "string") (:type "string" :enum ["true" "false"])] :default "true"))
     :required ["name" "switch" "validateName"]))])

(defvar gptel-test-tools-gemini
  [(:functionDeclarations
    [(:name "read_url" :description "Fetch and read the contents of a URL"
      :parameters
      (:type "object" :properties
       (:url (:description "The URL to read" :type "string")) :required ["url"]))
     (:name "get_weather" :description
      "Get the current weather in a given location" :parameters
      (:type "object" :properties
       (:location
        (:description
         "The latitude and longitude, in degrees.\nSouth and West (resp) are negative."
         :type "object" :properties
         (:lat (:type "number" :description "Latitude, [-90.0, 90.0]") :lon
          (:type "number" :description "Longitude, [-180.0, 180.0]"))
         :required ["lat" "lon"])
        :unit
        (:description "The unit of temperature, either 'celsius' or 'fahrenheit'"
         :type "string" :enum ["celsius" "farenheit"]))
       :required ["location"]))
     (:name "record_summary" :description
      "Record summary of an image using well-structured JSON." :parameters
      (:type "object" :properties
       (:key_colors
        (:description "Key colors in the image.  Limit to less than four." :type
         "array" :items
         (:type "object" :properties
          (:r (:type "number" :description "red value [0.0, 1.0]") :g
           (:type "number" :description "green value [0.0, 1.0]") :b
           (:type "number" :description "blue value [0.0, 1.0]") :name
           (:type "string" :description:
            "Human-readable color name in snake_case, e.g. \"olive_green\" or \"turquoise\""))
          :required ["r" "g" "b" "name"]))
        :description
        (:description "Image description.  One to two sentences max." :type
         "string")
        :estimated_year
        (:description
         "Estimated year that the images was taken, if is it a photo. Only set this if the image appears to be non-fictional. Rough estimates are okay!"
         :type "integer"))
       :required ["key_colors" "description"]))
     (:name "create_pull_request_review" :description "Create a review on a pull request" :parameters
      (:type "object" :properties
       (:body
        (:description "Review comment text" :type "string")
        :comments
        (:description "Line-specific comments array of objects to place comments on pull request changes. Requires path and body. For line comments use line or position. For multi-line comments use start_line and line with optional side parameters." :type "array" :items
         (:properties
          (:body
           (:description "comment body" :type "string")
           :line
           (:anyOf
            [(:type "number")
             (:type "null")]
            :description "line number in the file to comment on. For multi-line comments, the end of the line range")
           :path
           (:description "path to the file" :type "string")
           :position
           (:anyOf
            [(:type "number")
             (:type "null")]
            :description "position of the comment in the diff")
           :side
           (:anyOf
            [(:type "string")
             (:type "null")]
            :description "The side of the diff on which the line resides. For multi-line comments, this is the side for the end of the line range. (LEFT or RIGHT)")
           :start_line
           (:anyOf
            [(:type "number")
             (:type "null")]
            :description "The first line of the range to which the comment refers. Required for multi-line comments.")
           :start_side
           (:anyOf
            [(:type "string")
             (:type "null")]
            :description "The side of the diff on which the start line resides for multi-line comments. (LEFT or RIGHT)"))
          :required
          ["path" "body" "position" "line" "side" "start_line" "start_side"]
          :type "object"))
        :commitId
        (:description "SHA of commit to review" :type "string")
        :event
        (:description "Review action to perform" :enum
         ["APPROVE" "REQUEST_CHANGES" "COMMENT"]
         :type "string")
        :owner
        (:description "Repository owner" :type "string")
        :pullNumber
        (:description "Pull request number" :type "number")
        :repo
        (:description "Repository name" :type "string"))
       :required
       ["body" "comments" "commitId" "event" "owner" "pullNumber" "repo"]))
     (:name "create_database" :description
      "Create a new MongoDB database with option to switch" :parameters
      (:type "object" :properties
       (:name (:description "Database name to create" :type "string" :minLength 1)
        :switch
        (:description "Whether to switch to the new database after creation"
         :allOf [(:type "string") (:type "string" :enum ["true" "false"])] :default
         "false")
        :validateName
        (:description "Whether to validate database name" :allOf
         [(:type "string") (:type "string" :enum ["true" "false"])] :default "true"))
       :required ["name" "switch" "validateName"]))])])

(defmacro gptel-test-parse-tool-spec (backend-type parsed-tools)
  "Generate tool parsing test for BACKEND-TYPE, comparing with PARSED-TOOLS."
  `(ert-deftest ,(intern (concat "gptel-test-" (symbol-name backend-type) "-parse-tool-spec")) ()
    ,(concat "Check conversion between a list of gptel-tool specs and the tool definition sent
to backend type " (symbol-name backend-type) ".  Check against `gptel-test-tools'.")
    ;; We need a custom comparison function because some of the symbols in the
    ;; parsed list of specifications contain uninterned symbols, and `equal' will
    ;; fail.
    (cl-labels ((sym-equal (obj1 obj2)
                  (cond ((and (symbolp obj1) (symbolp obj2))
                         (equal (symbol-name obj1) (symbol-name obj2)))
                   ((and (vectorp obj1) (vectorp obj2))
                    (cl-loop for o1 across obj1
                     for o2 across obj2
                     always (sym-equal o1 o2)
                     finally return t))
                   ((and (consp obj1) (consp obj2))
                    (cl-loop for o1 in obj1
                     for o2 in obj2
                     always (sym-equal o1 o2)
                     finally return t))
                   ((and (atom obj1) (atom obj2))
                    (equal obj1 obj2))
                   ((and (null obj1) (null obj2))))))
     (let ((backend (alist-get ',backend-type gptel-test-backends)))
      (should (sym-equal (gptel--parse-tools backend gptel-test-tools)
               ,parsed-tools))))))

(gptel-test-parse-tool-spec openai gptel-test-tools-openai) ;covers Ollama as well
(gptel-test-parse-tool-spec anthropic gptel-test-tools-anthropic)
(gptel-test-parse-tool-spec gemini gptel-test-tools-gemini)
