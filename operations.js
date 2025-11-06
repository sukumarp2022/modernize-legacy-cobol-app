/**
 * Business Operations Module
 * Equivalent to operations.cob (Operations)
 * Handles credit, debit, and view balance operations
 */

const dataProgram = require('./data.js');
const readlineSync = require('readline-sync');

class Operations {
  /**
   * Process an operation based on the operation type
   * @param {string} operationType - 'TOTAL', 'CREDIT', or 'DEBIT'
   */
  processOperation(operationType) {
    // MOVE PASSED-OPERATION TO OPERATION-TYPE
    const operation = operationType.trim().toUpperCase();

    // IF OPERATION-TYPE = 'TOTAL '
    if (operation === 'TOTAL') {
      this.viewBalance();
    }
    // ELSE IF OPERATION-TYPE = 'CREDIT'
    else if (operation === 'CREDIT') {
      this.creditAccount();
    }
    // ELSE IF OPERATION-TYPE = 'DEBIT '
    else if (operation === 'DEBIT') {
      this.debitAccount();
    }
  }

  /**
   * View current balance
   * Equivalent to operations.cob lines 16-18
   */
  viewBalance() {
    // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    const finalBalance = dataProgram.readBalance();
    
    // DISPLAY "Current balance: " FINAL-BALANCE
    console.log(`Current balance: ${finalBalance.toFixed(2)}`);
  }

  /**
   * Credit account with an amount
   * Equivalent to operations.cob lines 20-26
   */
  creditAccount() {
    // DISPLAY "Enter credit amount: "
    console.log('Enter credit amount: ');
    
    // ACCEPT AMOUNT
    const amountStr = readlineSync.question('');
    const amount = parseFloat(amountStr) || 0;

    // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    let finalBalance = dataProgram.readBalance();
    
    // ADD AMOUNT TO FINAL-BALANCE
    finalBalance = finalBalance + amount;
    
    // CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
    dataProgram.writeBalance(finalBalance);
    
    // DISPLAY "Amount credited. New balance: " FINAL-BALANCE
    console.log(`Amount credited. New balance: ${finalBalance.toFixed(2)}`);
  }

  /**
   * Debit account with an amount
   * Equivalent to operations.cob lines 28-38
   */
  debitAccount() {
    // DISPLAY "Enter debit amount: "
    console.log('Enter debit amount: ');
    
    // ACCEPT AMOUNT
    const amountStr = readlineSync.question('');
    const amount = parseFloat(amountStr) || 0;

    // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    let finalBalance = dataProgram.readBalance();
    
    // IF FINAL-BALANCE >= AMOUNT
    if (finalBalance >= amount) {
      // SUBTRACT AMOUNT FROM FINAL-BALANCE
      finalBalance = finalBalance - amount;
      
      // CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
      dataProgram.writeBalance(finalBalance);
      
      // DISPLAY "Amount debited. New balance: " FINAL-BALANCE
      console.log(`Amount debited. New balance: ${finalBalance.toFixed(2)}`);
    } else {
      // DISPLAY "Insufficient funds for this debit."
      console.log('Insufficient funds for this debit.');
    }
  }
}

// Export a singleton instance
module.exports = new Operations();
