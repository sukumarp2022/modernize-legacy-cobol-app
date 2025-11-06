/**
 * Main Program
 * Equivalent to main.cob (MainProgram)
 * Entry point and user interface for the Account Management System
 */

const operations = require('./operations.js');
const readlineSync = require('readline-sync');

class MainProgram {
  constructor() {
    // CONTINUE-FLAG PIC X(3) VALUE 'YES'
    this.continueFlag = 'YES';
  }

  /**
   * Main program logic
   * Equivalent to main.cob MAIN-LOGIC section
   */
  run() {
    // PERFORM UNTIL CONTINUE-FLAG = 'NO'
    while (this.continueFlag === 'YES') {
      // Display menu
      console.log('--------------------------------');
      console.log('Account Management System');
      console.log('1. View Balance');
      console.log('2. Credit Account');
      console.log('3. Debit Account');
      console.log('4. Exit');
      console.log('--------------------------------');
      console.log('Enter your choice (1-4): ');

      // ACCEPT USER-CHOICE
      const userChoiceStr = readlineSync.question('');
      const userChoice = parseInt(userChoiceStr) || 0;

      // EVALUATE USER-CHOICE
      switch (userChoice) {
        case 1:
          // WHEN 1
          // CALL 'Operations' USING 'TOTAL '
          operations.processOperation('TOTAL');
          break;
        
        case 2:
          // WHEN 2
          // CALL 'Operations' USING 'CREDIT'
          operations.processOperation('CREDIT');
          break;
        
        case 3:
          // WHEN 3
          // CALL 'Operations' USING 'DEBIT '
          operations.processOperation('DEBIT');
          break;
        
        case 4:
          // WHEN 4
          // MOVE 'NO' TO CONTINUE-FLAG
          this.continueFlag = 'NO';
          break;
        
        default:
          // WHEN OTHER
          // DISPLAY "Invalid choice, please select 1-4."
          console.log('Invalid choice, please select 1-4.');
          break;
      }
    }
    
    // DISPLAY "Exiting the program. Goodbye!"
    console.log('Exiting the program. Goodbye!');
    
    // STOP RUN
    process.exit(0);
  }
}

// Start the application if this is the main module
if (require.main === module) {
  const app = new MainProgram();
  app.run();
}

module.exports = MainProgram;
