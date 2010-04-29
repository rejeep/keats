Feature: Edit keat
  In order to manage keats
  As an Emacs user
  I want to edit keats
  
  Background:
    Given I enable keats-mode
          
  Scenario: Edit keat
    Given I have one keat with key "C-x b" and description "old"
    When I start an action chain
    And I press "C-c k e"
    And I press "C-x b"
    And I press "RET"
    And I type "new"
    And I press "RET"
    And I execute the action chain
    Then I should have a keat with key "C-x b" and description "oldnew"

  Scenario: Edit non existing keat
    When I start an action chain
    And I press "C-c k e"
    And I press "C-x b"
    And I press "RET"
    And I execute the action chain
    Then I should see message "No keat with key C-x b exists"
