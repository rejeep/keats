Feature: Keats Mode
  In order to manage Keats
  As an Emacs user
  I want a simple interface to the Keats

  Scenario: Disable prefix key
    Given I enable keats-mode
    Then the prefix should be enabled
    When I disable keats-mode
    Then the prefix should be disabled
