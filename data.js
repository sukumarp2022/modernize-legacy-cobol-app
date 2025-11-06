/**
 * Data Management Module
 * Equivalent to data.cob (DataProgram)
 * Manages the storage of the account balance
 */

class DataProgram {
  constructor() {
    // STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00
    this.storageBalance = 1000.00;
  }

  /**
   * Handle read and write operations for balance
   * @param {string} operationType - 'READ' or 'WRITE'
   * @param {number|null} balance - Balance value for write operations
   * @returns {number} - Current balance for read operations
   */
  handleOperation(operationType, balance = null) {
    // MOVE PASSED-OPERATION TO OPERATION-TYPE
    const operation = operationType.trim().toUpperCase();

    // IF OPERATION-TYPE = 'READ'
    if (operation === 'READ') {
      // MOVE STORAGE-BALANCE TO BALANCE
      return this.storageBalance;
    }
    // ELSE IF OPERATION-TYPE = 'WRITE'
    else if (operation === 'WRITE') {
      // MOVE BALANCE TO STORAGE-BALANCE
      if (balance !== null) {
        this.storageBalance = balance;
      }
      return this.storageBalance;
    }

    return this.storageBalance;
  }

  /**
   * Read the current balance
   * @returns {number} Current balance
   */
  readBalance() {
    return this.handleOperation('READ');
  }

  /**
   * Write a new balance value
   * @param {number} balance - New balance value
   */
  writeBalance(balance) {
    this.handleOperation('WRITE', balance);
  }
}

// Export a singleton instance to maintain state across the application
module.exports = new DataProgram();
