Feature: Keats Mode
  In order to manage Keats
  As an Emacs user
  I want a simple interface to the Keats
  
  Background:
    Given I enable keats-mode
  
  Scenario: Disable prefix key
    Given I enable keats-mode
    Then the prefix should be enabled
    When I disable keats-mode
    Then the prefix should be disabled
    
  Scenario: Add new keat
    Given I start an action chain
    When I press "C-c k n"
    And I press "C-x b"
    And I press "RET"
    And I type "Switches to another buffer"
    And I press "RET"
    And I execute the action chain
    Then I should have one keat with key "C-x b" and description "Switches to another buffer"
    And I should see message "Successfully added keat for C-x b"

  Scenario: Abort adding new keat in key binding phase
    Given I start an action chain
    When I press "C-c k n"
    And I press "C-x b"
    And I press "C-g"
    And I execute the action chain
    Then I should have 0 keats
    
  # TODO: How to make this work?
  # Scenario: Abort adding new keat in description phase
  #   Given I start an action chain
  #   When I press "C-c k n"
  #   And I press "C-x b"
  #   And I press "RET"
  #   And I type "..."
  #   And I press "C-g"
  #   And I execute the action chain
  #   Then I should have 0 keats

  Scenario: Invalid key binding
    Given I start an action chain
    When I press "C-c k n"
    And I press "RET"
    And I press "RET"
    And I type "Valid"
    And I press "RET"
    And I execute the action chain
    Then I should have 0 keats
    And I should see message "Keat is invalid and was not added"
    
  Scenario: Invalid description
    Given I start an action chain
    When I press "C-c k n"
    And I press "C-x b"
    And I press "RET"
    # No typing
    And I press "RET"
    And I execute the action chain
    Then I should have 0 keats
    And I should see message "Keat is invalid and was not added"
