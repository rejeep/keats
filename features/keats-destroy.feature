Feature: Destroy keat
  In order to manage keats
  As an Emacs user
  I want to destroy keats
  
  Background:
    Given I enable keats-mode

  Scenario: Destroy keat
    Given I have one keat with key "C-x b" and description "Switches to another buffer"
    When I start an action chain
    And I press "C-c k d"
    And I press "C-x b"
    And I press "RET"
    And I type "yes"
    And I press "RET"
    And I execute the action chain
    Then I should have 0 keats
    And I should see message "Successfully destroyed keat "C-x b""

  Scenario: Change of mind when destroying keat
    Given I have one keat with key "C-x b" and description "Switches to another buffer"
    When I start an action chain
    And I press "C-c k d"
    And I press "C-x b"
    And I press "RET"
    And I type "no"
    And I press "RET"
    And I execute the action chain
    Then I should have 1 keat

  Scenario: Destroy non existing keat
    When I start an action chain
    And I press "C-c k d"
    And I press "C-x b"
    And I press "RET"
    And I execute the action chain
    Then I should see message "No keat with key "C-x b" exists"
