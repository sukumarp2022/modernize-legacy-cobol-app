# Testing Guide for Node.js Account Management System

This guide provides manual testing instructions to verify that the Node.js application correctly implements all business logic from the COBOL source.

## Prerequisites

1. Node.js installed (v14.0.0 or higher)
2. Dependencies installed: `npm install`

## Quick Test Commands

### Test 1: Verify Application Starts and Initial Balance

```bash
# Start the application
npm start

# When prompted, enter:
1  # View Balance

# Expected output:
# Current balance: 1000.00

# Then enter:
4  # Exit

# Expected output:
# Exiting the program. Goodbye!
```

### Test 2: Credit Account Operation

```bash
npm start

# Test sequence:
2      # Credit Account
500    # Enter amount
1      # View Balance (should show 1500.00)
4      # Exit
```

Expected outputs:
```
Enter credit amount: 
500
Amount credited. New balance: 1500.00
Current balance: 1500.00
```

### Test 3: Debit Account Operation

```bash
npm start

# Test sequence:
3      # Debit Account  
300    # Enter amount
1      # View Balance (should show 700.00)
4      # Exit
```

Expected outputs:
```
Enter debit amount:
300
Amount debited. New balance: 700.00
Current balance: 700.00
```

### Test 4: Insufficient Funds

```bash
npm start

# Test sequence:
3      # Debit Account
2000   # Enter amount greater than balance
1      # View Balance (should still show 1000.00)
4      # Exit
```

Expected outputs:
```
Enter debit amount:
2000
Insufficient funds for this debit.
Current balance: 1000.00
```

### Test 5: Invalid Menu Choice

```bash
npm start

# Test sequence:
9      # Invalid choice
# Expected: "Invalid choice, please select 1-4."
1      # View Balance
4      # Exit
```

### Test 6: Complete Workflow (Test Case TC-4.1)

```bash
npm start

# Test sequence:
1       # View Balance (should show 1000.00)
2       # Credit Account
500     # Amount
1       # View Balance (should show 1500.00)
3       # Debit Account
300     # Amount  
1       # View Balance (should show 1200.00)
4       # Exit
```

Expected final balance: 1200.00

## Test Coverage Matrix

This testing guide covers the following test cases from TESTPLAN.md:

| Test Case | Description | Command Sequence |
|-----------|-------------|------------------|
| TC-1.1.1 | Verify Initial Balance | Test 1 |
| TC-1.2.1 | View Initial Balance | Test 1 |
| TC-1.3.1 | Credit with Valid Amount | Test 2 |
| TC-1.4.1 | Debit with Valid Amount | Test 3 |
| TC-1.4.5 | Debit with Insufficient Funds | Test 4 |
| TC-3.1/TC-3.2 | Invalid Menu Choice | Test 5 |
| TC-4.1 | Complete User Workflow | Test 6 |

## Automated Testing

For automated test execution, see Issue #6: Create Automated Tests

The automated tests will use Jest or Mocha testing framework to verify:
- Unit tests for each module (data.js, operations.js, main.js)
- Integration tests for complete workflows
- Boundary condition tests
- Input validation tests

## Business Logic Verification

All business rules from the COBOL application are preserved:

✅ Initial balance: $1,000.00  
✅ Overdraft protection (balance cannot go negative)  
✅ Balance range: $0.00 to $999,999.99  
✅ Decimal precision: 2 places  
✅ Menu validation: Only 1-4 accepted  
✅ Zero amount transactions allowed  
✅ Balance persistence during session  

## Comparison with COBOL Version

To verify the Node.js version matches the COBOL version:

1. Compile and run the COBOL version:
```bash
cobc -x main.cob operations.cob data.cob -o accountsystem
./accountsystem
```

2. Run the Node.js version:
```bash
npm start
```

3. Execute the same test sequence in both versions
4. Verify outputs match exactly

## Troubleshooting

### Issue: "Cannot find module 'readline-sync'"

**Solution:** Run `npm install` to install dependencies

### Issue: "permission denied: ./main.js"

**Solution:** Use `node main.js` instead of executing directly

### Issue: Balance not persisting

**Note:** This is expected behavior. Balance resets to $1,000.00 on each application restart, matching the COBOL implementation.

## Next Steps

After manual testing confirms functionality:
1. Create automated unit tests (Issue #6)
2. Create integration tests
3. Set up CI/CD pipeline
4. Add code coverage reporting

## References

- Full Test Plan: [TESTPLAN.md](TESTPLAN.md)
- Node.js Documentation: [README-NODEJS.md](README-NODEJS.md)
- COBOL Documentation: [COBOL_DOCUMENTATION.md](COBOL_DOCUMENTATION.md)
