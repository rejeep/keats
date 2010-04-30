Feature: Add new keat
  In order to manage keats
  As an Emacs user
  I want to add keats
  
  Background:
    Given I enable keats-mode
    
  Scenario: Add new keat
    When I start an action chain
    And I press "C-c k n"
    And I press "C-x b"
    And I press "RET"
    And I type "Switches to another buffer"
    And I press "RET"
    And I execute the action chain
    Then I should have a keat with key "C-x b" and description "Switches to another buffer"
    And I should see message "Successfully added keat for C-x b"

  Scenario: Abort adding new keat in key binding phase
    When I start an action chain
    And I press "C-c k n"
    And I press "C-x b"
    And I press "C-g"
    And I execute the action chain
    Then I should have 0 keats

  Scenario: Add new keat with invalid key binding
    When I start an action chain
    And I press "C-c k n"
    And I press "RET"
    And I press "RET"
    And I type "Valid"
    And I press "RET"
    And I execute the action chain
    Then I should have 0 keats
    And I should see message "Keat is invalid and was not added"
    
  Scenario: Add new keat with invalid description
    When I start an action chain
    And I press "C-c k n"
    And I press "C-x b"
    And I press "RET"
    # No typing
    And I press "RET"
    And I execute the action chain
    Then I should have 0 keats
    And I should see message "Keat is invalid and was not added"

  # Scenario: Add new already existing keat
  #   Given I have one keat with key "C-x b" and description "Switches to another buffer"
  #   When I start an action chain
  #   And I press "C-c k n"
  #   And I press "C-x b"
  #   And I press "RET"
  #   And I execute the action chain
  #   Then I should see message "Keat for key C-x b already defined"
  #   And I should have keat with key "C-x b" and description "Switches to another buffer"
  
  Scenario: Abort adding new keat in description phase
    Given I start an action chain
    When I press "C-c k n"
    And I press "C-x b"
    And I press "RET"
    And I type "..."
    And I press "C-g"
    And I execute the action chain
    Then I should have 0 keats
