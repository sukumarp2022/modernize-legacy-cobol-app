# Account Management System - Node.js Version

This is the modernized Node.js version of the COBOL Account Management System.

## Installation

1. Install Node.js dependencies:
```bash
npm install
```

## Usage

Run the application:
```bash
npm start
```

Or directly:
```bash
node main.js
```

## Features

- View Balance - Display current account balance
- Credit Account - Add funds to the account
- Debit Account - Withdraw funds from the account
- Exit - Safely terminate the application

## Initial Balance

The application starts with an initial balance of $1000.00

## Files

- `main.js` - Main program logic and user interface
- `operations.js` - Business operations (credit, debit, view balance)
- `data.js` - Data management and storage

## Business Rules

- Starting balance: $1000.00
- Balance cannot go negative (overdraft protection)
- Balance range: $0.00 to $999,999.99
- Decimal precision: 2 places (cents)
- Zero amount credits/debits are allowed but don't change balance
