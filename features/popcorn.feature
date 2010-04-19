Feature: Popcorn
  In order to manage Keats through Popcorn
  As an Emacs user
  I want a simple interface to the Keats

  Scenario: Add new keat
    Given I start an action chain
    When I press "C-c k n"
    And I press "C-x b"
    And I press "RET"
    And I type "Switches to another buffer"
    And I press "RET"
    And I execute the action chain
    Then I should have one keat with key "C-x b" and description "Switches to another buffer"
