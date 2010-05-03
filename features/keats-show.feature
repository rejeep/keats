Feature: Show keat description
  In order to manage keats
  As an Emacs user
  I want to show keat descriptions
  
  Background:
    Given I enable keats-mode
    
  Scenario: Show keat description
    Given I have one keat with key "C-x b" and description "Switches to another buffer"
    When I start an action chain
    And I press "C-c k s"
    And I press "C-x b"
    And I press "RET"
    And I execute the action chain
    Then I should see message "Switches to another buffer"

  Scenario: Show non existing keat description
    When I start an action chain
    And I press "C-c k s"
    And I press "C-x b"
    And I press "RET"
    And I execute the action chain
    Then I should see message "No keat with key "C-x b" exists"
