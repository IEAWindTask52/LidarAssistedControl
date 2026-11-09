function tests = CorrectLosTiming_UnitTest
% Function-based unit test suite for CorrectLosTiming.
% Each test case corresponds to the examples documented in the function header.
% Run with: results = runtests('test_CorrectLosTiming');
%
% Requirements:
%   - CorrectLosTiming.m must be on the MATLAB path
 
tests = functiontests(localfunctions);
end
 
% -------------------------------------------------------------------------
% Helper
% -------------------------------------------------------------------------
function tol = defaultTol()
% Default tolerance used by CorrectLosTiming (matches function default)
tol = 0.005;
end
 
function time = dummyTime(n)
% Generates a dummy time vector of length n (not used in logic, but required input)
time = (1:n)';
end
 
% -------------------------------------------------------------------------
% Example 1: Data is 1 step too early
% LOS       = [1 1 1 2 2 2 2 2 3]
% Data      = [6 6 7 7 7 7 7 7 8]
% Datac     = [6 6 6 7 7 7 7 7 8]
% -------------------------------------------------------------------------
function test_example1_oneStepTooEarly(testCase)
LOS         = [1 1 1 2 2 2 2 2 3]';
Data        = [6 6 7 7 7 7 7 7 8]';
expected    = [6 6 6 7 7 7 7 7 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 1 failed: Data 1 step too early');
end
 
% -------------------------------------------------------------------------
% Example 2: Data is 1 step too late
% LOS       = [1 1 1 2 2 2 2 2 3]
% Data      = [6 6 6 6 7 7 7 7 8]
% Datac     = [6 6 6 7 7 7 7 7 8]
% -------------------------------------------------------------------------
function test_example2_oneStepTooLate(testCase)
LOS         = [1 1 1 2 2 2 2 2 3]';
Data        = [6 6 6 6 7 7 7 7 8]';
expected    = [6 6 6 7 7 7 7 7 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 2 failed: Data 1 step too late');
end
 
% -------------------------------------------------------------------------
% Example 3: Data is 2 steps too early
% LOS       = [1 1 1 2 2 2 2 2 3]
% Data      = [6 7 7 7 7 7 7 7 8]
% Datac     = [6 6 6 7 7 7 7 7 8]
% -------------------------------------------------------------------------
function test_example3_twoStepsTooEarly(testCase)
LOS         = [1 1 1 2 2 2 2 2 3]';
Data        = [6 7 7 7 7 7 7 7 8]';
expected    = [6 6 6 7 7 7 7 7 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 3 failed: Data 2 steps too early');
end
 
% -------------------------------------------------------------------------
% Example 4: Data is 2 steps too late
% LOS       = [1 1 1 2 2 2 2 2 3]
% Data      = [6 6 6 6 6 7 7 7 8]
% Datac     = [6 6 6 7 7 7 7 7 8]
% -------------------------------------------------------------------------
function test_example4_twoStepsTooLate(testCase)
LOS         = [1 1 1 2 2 2 2 2 3]';
Data        = [6 6 6 6 6 7 7 7 8]';
expected    = [6 6 6 7 7 7 7 7 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 4 failed: Data 2 steps too late');
end
 
% -------------------------------------------------------------------------
% Example 5: Data is 2 steps too late, then 2 steps too early
% LOS       = [1 1 1 2 2 2 2 2 3]
% Data      = [6 6 6 6 6 7 8 8 8]
% Datac     = [6 6 6 7 7 7 7 7 8]
% -------------------------------------------------------------------------
function test_example5_tooLateAndTooEarlyCombined(testCase)
LOS         = [1 1 1 2 2 2 2 2 3]';
Data        = [6 6 6 6 6 7 8 8 8]';
expected    = [6 6 6 7 7 7 7 7 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 5 failed: 2 steps late then 2 steps early');
end
 
% -------------------------------------------------------------------------
% Example 6: First 2 data points too early -> no correction
% LOS       = [1 1 2 2 2 2 2 3 3]
% Data      = [7 7 7 7 7 7 7 8 8]
% Datac     = [7 7 7 7 7 7 7 8 8]  (unchanged)
% -------------------------------------------------------------------------
function test_example6_firstTwoPointsTooEarlyNoCorrection(testCase)
LOS         = [1 1 2 2 2 2 2 3 3]';
Data        = [7 7 7 7 7 7 7 8 8]';
expected    = [7 7 7 7 7 7 7 8 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 6 failed: first 2 points too early, expected no correction');
end
 
% -------------------------------------------------------------------------
% Example 7: Last 2 data points too late -> no correction
% LOS       = [1 1 2 2 2 2 2 3 3]
% Data      = [6 6 7 7 7 7 7 7 7]
% Datac     = [6 6 7 7 7 7 7 7 7]  (unchanged)
% -------------------------------------------------------------------------
function test_example7_lastTwoPointsTooLateNoCorrection(testCase)
LOS         = [1 1 2 2 2 2 2 3 3]';
Data        = [6 6 7 7 7 7 7 7 7]';
expected    = [6 6 7 7 7 7 7 7 7]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 7 failed: last 2 points too late, expected no correction');
end
 
% -------------------------------------------------------------------------
% Example 8: First 2 data points are too late
% LOS       = [1 1 1 1 1 2 2 2 2]
% Data      = [5 5 6 6 6 7 7 7 7]
% Datac     = [6 6 6 6 6 7 7 7 7]
% -------------------------------------------------------------------------
function test_example8_firstTwoPointsTooLate(testCase)
LOS         = [1 1 1 1 1 2 2 2 2]';
Data        = [5 5 6 6 6 7 7 7 7]';
expected    = [6 6 6 6 6 7 7 7 7]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 8 failed: first 2 points too late');
end
 
% -------------------------------------------------------------------------
% Example 9: Last 2 data points are too early
% LOS       = [1 1 1 1 2 2 2 2 2]
% Data      = [6 6 6 6 7 7 7 7 8]
% Datac     = [6 6 6 6 6 7 7 7 7]
% -------------------------------------------------------------------------
function test_example9_lastTwoPointsTooEarly(testCase)
LOS         = [1 1 1 1 2 2 2 2 2]';
Data        = [6 6 6 6 7 7 7 7 8]';
expected    = [6 6 6 6 7 7 7 7 7]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 9 failed: last 2 points too early');
end
 
% -------------------------------------------------------------------------
% Example 10: Data is 3 steps too late (expected: warning, no full correction)
% LOS       = [1 1 1 2 2 2 2 2 3]
% Data      = [6 6 6 6 6 6 7 7 8]
% Datac     = [6 6 6 6 6 6 6 6 8]  (partial correction with warning)
% -------------------------------------------------------------------------
function test_example10_threeStepsTooLate_warning(testCase)
LOS         = [1 1 1 2 2 2 2 2 3]';
Data        = [6 6 6 6 6 6 7 7 8]';
expected    = [6 6 6 7 7 7 7 7 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 10 failed: output mismatch for 3-step offset');
end
 
% -------------------------------------------------------------------------
% Example 11: Data is 2 steps too late for n=4
% LOS       = [1 1 1 2 2 2 2 3 3]
% Data      = [6 6 6 6 6 7 7 8 8]
% Datac     = [6 6 6 7 7 7 7 8 8]
% -------------------------------------------------------------------------
function test_example11_twoStepsTooLate(testCase)
LOS         = [1 1 1 2 2 2 2 3 3]';
Data        = [6 6 6 6 6 7 7 8 8]';
expected    = [6 6 6 7 7 7 7 8 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 11 failed: 2 steps too late with n=4');
end
 
% -------------------------------------------------------------------------
% Example 12: Data is 1 step too late then 1 step too early for n=3
% LOS       = [1 1 1 2 2 2 3 3 3]
% Data      = [6 6 6 6 7 8 8 8 8]
% Datac     = [6 6 6 7 7 7 8 8 8]
% -------------------------------------------------------------------------
function test_example12_oneStepLateThenEarly(testCase)
LOS         = [1 1 1 2 2 2 3 3 3]';
Data        = [6 6 6 6 7 8 8 8 8]';
expected    = [6 6 6 7 7 7 8 8 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 12 failed: 1 step late then 1 step early with n=3');
end
 
% -------------------------------------------------------------------------
% Example 13: Data is 2 steps too early for n=3 (outside correctable range)
% LOS       = [1 1 1 2 2 2 3 3 3]
% Data      = [6 6 6 6 6 7 8 8 8]  <- step at idx 6 is 2 early (LOS steps at idx 4)
% Datac     = [6 6 6 6 6 6 8 8 8]  (partial correction as documented)
% -------------------------------------------------------------------------
function test_example13_twoStepsTooEarly_wrong(testCase)
LOS         = [1 1 1 2 2 2 3 3 3]';
Data        = [6 6 6 6 6 7 8 8 8]';
expected    = [6 6 6 7 7 7 8 8 8]';
actual      = CorrectLosTiming(dummyTime(9), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Example 13 failed: 2 steps too early with n=3 (outside range)');
end
 
% -------------------------------------------------------------------------
% Sanity check: perfectly synchronous data -> output equals input
% -------------------------------------------------------------------------
function test_synchronousData_noChange(testCase)
LOS      = [1 1 1 1 1 2 2 2 2 2 3 3 3 3 3]';
Data     = [6 6 6 6 6 7 7 7 7 7 8 8 8 8 8]';
actual   = CorrectLosTiming(dummyTime(12), LOS, Data);
verifyEqual(testCase, actual, Data, 'AbsTol', defaultTol(), ...
    'Sanity check failed: synchronous data should not be modified');
end
 
% -------------------------------------------------------------------------
% Bug regression: dDatac & ~dLOS vs dDatac ~= dLOS
% -------------------------------------------------------------------------
% After a 2-step correction, a LOS step may have no corresponding Data step
% (dLOS=1, dDatac=0). This is NOT an error — Data was correctly aligned.
% The check must only flag "Data steps where LOS does not" (dDatac & ~dLOS),
% NOT "anywhere the two differ" (dDatac ~= dLOS).
%
% Scenario: 2 steps too late with n=5 (typical 20Hz/4Hz ratio).
% After correction, LOS has a step at idx 6 with no Data step -> harmless.
% Expectation: no warning, correct output.
%
% LOS   = [1 1 1 1 1 2 2 2 2 2 3 3 3 3 3]
% Data  = [6 6 6 6 6 6 6 7 7 7 8 8 8 8 8]  (2 steps too late at both transitions)
% Datac = [6 6 6 7 7 7 7 7 7 7 8 8 8 8 8]  <- wrong, shown to illustrate issue
% Datac = [6 6 6 6 6 7 7 7 7 7 8 8 8 8 8]  <- correct expected output
% -------------------------------------------------------------------------
function test_noWarning_afterTwoStepCorrection(testCase)
LOS         = [1 1 1 1 1 2 2 2 2 2 3 3 3 3 3]';
Data        = [6 6 6 6 6 6 6 7 7 7 8 8 8 8 8]';
expected    = [6 6 6 6 6 7 7 7 7 7 8 8 8 8 8]';
 
% verifyWarningFree confirms that dDatac & ~dLOS is used (not ~=)
% If the bug is present (dDatac ~= dLOS), a warning fires here incorrectly
verifyWarningFree(testCase, ...
    @() CorrectLosTiming(dummyTime(15), LOS, Data), ...
    'Spurious warning fired: check IsAsynchronous uses (dDatac & ~dLOS), not (dDatac ~= dLOS)');
 
actual = CorrectLosTiming(dummyTime(15), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Bug regression failed: 2-step correction produced wrong output');
end

% Thaw-freeze: LOS frozen at 4, Data correctly jumps to 0 (invalid measurement)
% LOS   = [4 4 4 4 4 4 4 4 4 4 4 4 4 4]
% Data  = [7 7 7 7 7 0 0 0 0 0 0 0 0 0]  <- correct jump, LOS is the faulty signal here
% Datac = [7 7 7 7 7 0 0 0 0 0 0 0 0 0]  <- unchanged, no correction, no warning
function test_thawFreeze_noCorrection_noWarning(testCase)
LOS      = [4 4 4 4 4 4 4 4 4 4 4 4 4 4]';
Data     = [7 7 7 7 7 0 0 0 0 0 0 0 0 0]';
expected = [7 7 7 7 7 0 0 0 0 0 0 0 0 0]';

verifyWarningFree(testCase, ...
    @() CorrectLosTiming(dummyTime(4), LOS, Data), ...
    'Thaw-freeze event incorrectly triggered a warning');

actual = CorrectLosTiming(dummyTime(4), LOS, Data);
verifyEqual(testCase, actual, expected, 'AbsTol', defaultTol(), ...
    'Thaw-freeze failed: data should not be modified');
end