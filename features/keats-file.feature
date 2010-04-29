Feature: Keats file
  In order preserve keats between Emacs sessions
  As an Emacs user
  I want to store the keats in a file
          
  Scenario: Load keats file
    When I load this keats file:
      """
      (
       [cl-struct-keats-keat "C-x b" "Switches to another buffer"]
       [cl-struct-keats-keat "C-x C-b" "Display a list of names of existing buffers"]
       )
      """
    Then I should have these keats:
      | key     | description                                 |
      | C-x b   | Switches to another buffer                  |
      | C-x C-b | Display a list of names of existing buffers |

  Scenario: Create keats file on load if not exist
    Given the keats-file is set to a non existing file
    When I load the keats file
    Then I should have 0 keats
    And I should have a keats file

  Scenario: Load keats file in old format
    When I load this keats file:
      """
      (
       (:key "C-x b"   :description "Switches to another buffer")
       (:key "C-x C-b" :description "Display a list of names of existing buffers")
       )
      """
    Then I should have these keats:
      | key     | description                                 |
      | C-x b   | Switches to another buffer                  |
      | C-x C-b | Display a list of names of existing buffers |

  Scenario: Load empty keats file
    When I load this keats file:
      """
      """
    Then I should have 0 keats
