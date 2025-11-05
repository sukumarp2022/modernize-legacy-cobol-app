# COBOL Application Documentation

## 📋 Table of Contents
1. [High-Level Application Overview](#high-level-application-overview)
2. [Application Architecture](#application-architecture)
3. [File-by-File Analysis](#file-by-file-analysis)
4. [Business Logic Summary](#business-logic-summary)
5. [Data Structures](#data-structures)
6. [File Interactions and Control Flow](#file-interactions-and-control-flow)
7. [Business Rules and Validation Logic](#business-rules-and-validation-logic)
8. [Execution Flow Map](#execution-flow-map)

---

## High-Level Application Overview

### Purpose
This COBOL application is an **Account Management System** that provides basic banking operations. It allows users to manage a single account by performing credit, debit, and balance inquiry operations through an interactive menu-driven interface.

### Main Features
1. **View Balance** - Display the current account balance
2. **Credit Account** - Add funds to the account
3. **Debit Account** - Withdraw funds from the account (with insufficient funds protection)
4. **Exit** - Safely terminate the application

### Application Type
- **Interactive console application** with menu-based user interface
- **Modular design** with separation of concerns across three COBOL programs
- **Stateful** - maintains account balance throughout the session using a data management layer

### User Interaction Flow
1. User starts the application and sees a menu with 4 options
2. User selects an operation (1-4)
3. Application processes the request and displays results
4. Menu is redisplayed until user chooses to exit
5. Application terminates gracefully with a goodbye message

---

## Application Architecture

### Three-Tier Architecture

```
┌─────────────────────────────────────────────┐
│          Presentation Layer                 │
│         (main.cob - MainProgram)            │
│   - User Interface & Menu Display           │
│   - Input Validation & Routing              │
└─────────────────┬───────────────────────────┘
                  │
                  │ CALL Operations
                  ▼
┌─────────────────────────────────────────────┐
│         Business Logic Layer                │
│       (operations.cob - Operations)         │
│   - Transaction Processing                  │
│   - Business Rule Implementation            │
│   - Calculation Logic                       │
└─────────────────┬───────────────────────────┘
                  │
                  │ CALL DataProgram
                  ▼
┌─────────────────────────────────────────────┐
│           Data Access Layer                 │
│       (data.cob - DataProgram)              │
│   - Balance Storage Management              │
│   - Read/Write Operations                   │
└─────────────────────────────────────────────┘
```

### Design Patterns
- **Layered Architecture** - Clear separation between presentation, business logic, and data layers
- **Command Pattern** - User choices trigger specific operation commands
- **Repository Pattern** - DataProgram acts as a repository for balance data

---

## File-by-File Analysis

### 1. main.cob - MainProgram

#### Purpose
The main entry point and presentation layer of the application. Responsible for displaying the user interface, capturing user input, and routing requests to the appropriate business logic handler.

#### Program Structure

**IDENTIFICATION DIVISION**
- Program ID: `MainProgram`
- Entry point for the entire application

**DATA DIVISION - WORKING-STORAGE SECTION**
```cobol
01  USER-CHOICE       PIC 9 VALUE 0.
01  CONTINUE-FLAG     PIC X(3) VALUE 'YES'.
```

**Data Fields:**
- `USER-CHOICE` (PIC 9): Single-digit numeric field to store user's menu selection (1-4)
  - Initial value: 0
  - Valid range: 1-4
  
- `CONTINUE-FLAG` (PIC X(3)): Control flag for the main program loop
  - Initial value: 'YES'
  - Values: 'YES' (continue) or 'NO' (exit)

**PROCEDURE DIVISION**

Main Section: `MAIN-LOGIC`

**Program Flow:**
1. **Loop Structure**: `PERFORM UNTIL CONTINUE-FLAG = 'NO'`
   - Continues until user selects exit option
   
2. **Menu Display**: Shows 4 formatted options with decorative borders
   ```
   --------------------------------
   Account Management System
   1. View Balance
   2. Credit Account
   3. Debit Account
   4. Exit
   --------------------------------
   ```

3. **User Input**: `ACCEPT USER-CHOICE` - Captures single-digit input

4. **Request Routing**: `EVALUATE USER-CHOICE` statement routes to appropriate operations:
   - **WHEN 1**: `CALL 'Operations' USING 'TOTAL '` - View balance
   - **WHEN 2**: `CALL 'Operations' USING 'CREDIT'` - Credit account
   - **WHEN 3**: `CALL 'Operations' USING 'DEBIT '` - Debit account
   - **WHEN 4**: `MOVE 'NO' TO CONTINUE-FLAG` - Set exit flag
   - **WHEN OTHER**: Display "Invalid choice, please select 1-4." - Input validation

5. **Termination**: After loop exits, displays "Exiting the program. Goodbye!" and executes `STOP RUN`

#### Key Responsibilities
- User interface management
- Input validation (ensures choice is 1-4)
- Control flow management
- Delegation to business logic layer (Operations)
- Session lifecycle management (start/exit)

#### External Dependencies
- Calls: `Operations` program for all transaction processing

#### Design Notes
- **Stateless**: Does not maintain any business data
- **Single Responsibility**: Only handles UI and routing
- **Loop Control**: Uses flag pattern for clean loop exit
- **String Padding**: Operation codes are padded to 6 characters ('TOTAL ', 'CREDIT', 'DEBIT ')

---

### 2. operations.cob - Operations

#### Purpose
The business logic layer that handles all account operations including balance inquiries, credits, and debits. This program implements the core business rules and transaction processing logic.

#### Program Structure

**IDENTIFICATION DIVISION**
- Program ID: `Operations`
- Callable subprogram invoked by MainProgram

**DATA DIVISION - WORKING-STORAGE SECTION**
```cobol
01 OPERATION-TYPE     PIC X(6).
01 AMOUNT             PIC 9(6)V99.
01 FINAL-BALANCE      PIC 9(6)V99 VALUE 1000.00.
```

**Working Storage Fields:**
- `OPERATION-TYPE` (PIC X(6)): Stores the operation code passed from MainProgram
  - Possible values: 'TOTAL ', 'CREDIT', 'DEBIT '
  
- `AMOUNT` (PIC 9(6)V99): Transaction amount for credit/debit operations
  - Format: 6 digits with 2 decimal places (e.g., 123456.99)
  - Maximum: 999,999.99
  
- `FINAL-BALANCE` (PIC 9(6)V99): Working variable for balance calculations
  - Initial value: 1000.00 (default starting balance)
  - Format: 6 digits with 2 decimal places

**DATA DIVISION - LINKAGE SECTION**
```cobol
01 PASSED-OPERATION   PIC X(6).
```

**Linkage Fields:**
- `PASSED-OPERATION` (PIC X(6)): Parameter passed from calling program
  - Receives operation code from MainProgram

**PROCEDURE DIVISION**

Entry Point: `PROCEDURE DIVISION USING PASSED-OPERATION`

**Operation Processing Logic:**

1. **Parameter Reception**: `MOVE PASSED-OPERATION TO OPERATION-TYPE`
   - Copies the operation code to working storage

2. **Operation Dispatch**: Three conditional branches based on operation type

**Branch 1: View Balance (TOTAL)**
```cobol
IF OPERATION-TYPE = 'TOTAL '
    CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    DISPLAY "Current balance: " FINAL-BALANCE
```
- Reads current balance from DataProgram
- Displays balance to user
- No modifications to account state

**Branch 2: Credit Account (CREDIT)**
```cobol
ELSE IF OPERATION-TYPE = 'CREDIT'
    DISPLAY "Enter credit amount: "
    ACCEPT AMOUNT
    CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    ADD AMOUNT TO FINAL-BALANCE
    CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
    DISPLAY "Amount credited. New balance: " FINAL-BALANCE
```
- Prompts user for credit amount
- Reads current balance
- Adds credit amount to balance
- Writes updated balance back to storage
- Displays confirmation with new balance
- **Note**: No validation for negative or zero amounts (accepts any numeric input)

**Branch 3: Debit Account (DEBIT)**
```cobol
ELSE IF OPERATION-TYPE = 'DEBIT '
    DISPLAY "Enter debit amount: "
    ACCEPT AMOUNT
    CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    IF FINAL-BALANCE >= AMOUNT
        SUBTRACT AMOUNT FROM FINAL-BALANCE
        CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        DISPLAY "Amount debited. New balance: " FINAL-BALANCE
    ELSE
        DISPLAY "Insufficient funds for this debit."
    END-IF
```
- Prompts user for debit amount
- Reads current balance
- **Validation**: Checks if balance is sufficient
- If sufficient:
  - Subtracts amount from balance
  - Writes updated balance
  - Displays confirmation
- If insufficient:
  - Displays error message
  - Balance remains unchanged

3. **Return**: `GOBACK` - Returns control to calling program

#### Key Responsibilities
- Transaction processing (credit/debit)
- Balance inquiry
- User interaction for amount input
- Business rule enforcement (insufficient funds check)
- Calculation logic (add/subtract operations)
- Delegation to data layer for persistence

#### External Dependencies
- Called by: `MainProgram`
- Calls: `DataProgram` for balance read/write operations

#### Business Rules Implemented
1. **Insufficient Funds Protection**: Prevents debit if amount exceeds balance
2. **Balance Persistence**: Always reads current balance before modification
3. **Atomic Updates**: Read-Modify-Write pattern ensures data consistency
4. **Transaction Confirmation**: Displays result of every operation

#### Design Notes
- **Stateful within transaction**: Uses FINAL-BALANCE as working variable
- **Data Layer Abstraction**: Doesn't know how DataProgram stores data
- **User Feedback**: Every operation provides clear confirmation messages
- **No Transaction Logging**: No audit trail of operations performed

---

### 3. data.cob - DataProgram

#### Purpose
The data access layer that manages the persistent storage of the account balance. Acts as a simple in-memory database with read and write operations.

#### Program Structure

**IDENTIFICATION DIVISION**
- Program ID: `DataProgram`
- Callable subprogram invoked by Operations

**DATA DIVISION - WORKING-STORAGE SECTION**
```cobol
01  STORAGE-BALANCE    PIC 9(6)V99 VALUE 1000.00.
01  OPERATION-TYPE     PIC X(6).
```

**Working Storage Fields:**
- `STORAGE-BALANCE` (PIC 9(6)V99): The single source of truth for account balance
  - Initial value: 1000.00 (starting balance)
  - Persists throughout application session
  - Format: 6 digits with 2 decimal places
  - **Scope**: Retains value across multiple CALL invocations
  
- `OPERATION-TYPE` (PIC X(6)): Stores the requested operation
  - Possible values: 'READ' or 'WRITE'

**DATA DIVISION - LINKAGE SECTION**
```cobol
01  PASSED-OPERATION   PIC X(6).
01  BALANCE            PIC 9(6)V99.
```

**Linkage Fields:**
- `PASSED-OPERATION` (PIC X(6)): Operation code from caller ('READ' or 'WRITE')
- `BALANCE` (PIC 9(6)V99): Balance value for read/write operations
  - For READ: Output parameter (DataProgram → Operations)
  - For WRITE: Input parameter (Operations → DataProgram)

**PROCEDURE DIVISION**

Entry Point: `PROCEDURE DIVISION USING PASSED-OPERATION BALANCE`

**Data Access Logic:**

1. **Parameter Reception**: `MOVE PASSED-OPERATION TO OPERATION-TYPE`
   - Copies operation code to working storage

2. **Operation Dispatch**: Two conditional branches

**Branch 1: Read Operation**
```cobol
IF OPERATION-TYPE = 'READ'
    MOVE STORAGE-BALANCE TO BALANCE
```
- Copies the current stored balance to the output parameter
- Non-destructive operation
- Returns current state to caller

**Branch 2: Write Operation**
```cobol
ELSE IF OPERATION-TYPE = 'WRITE'
    MOVE BALANCE TO STORAGE-BALANCE
```
- Copies the input parameter to storage
- Updates the persistent balance
- Overwrites previous value

3. **Return**: `GOBACK` - Returns control to calling program

#### Key Responsibilities
- Balance storage management
- Read operations (balance retrieval)
- Write operations (balance updates)
- Data persistence within session

#### External Dependencies
- Called by: `Operations` program
- No external calls

#### Data Persistence Model
- **In-Memory**: Balance stored in WORKING-STORAGE
- **Session-Scoped**: Balance persists across multiple calls within same session
- **Non-Durable**: Balance resets to 1000.00 on application restart
- **Single Account**: Only one balance maintained (no multi-account support)

#### Design Notes
- **Stateful**: Maintains state across invocations via WORKING-STORAGE
- **Simple Repository**: Implements basic CRUD (Create/Read/Update) pattern
- **No Validation**: Trusts caller to provide valid data
- **No Concurrency Control**: Not designed for multi-user access
- **Atomic Operations**: Each READ or WRITE is a single atomic operation

---

## Business Logic Summary

### Core Business Operations

#### 1. View Balance Operation
- **Trigger**: User selects menu option 1
- **Process**:
  1. MainProgram calls Operations with 'TOTAL '
  2. Operations calls DataProgram with 'READ'
  3. DataProgram returns current balance
  4. Operations displays balance to user
- **Business Rules**: None
- **Side Effects**: None (read-only operation)
- **Output**: Displays current balance

#### 2. Credit Account Operation
- **Trigger**: User selects menu option 2
- **Process**:
  1. MainProgram calls Operations with 'CREDIT'
  2. Operations prompts user for amount
  3. User enters amount
  4. Operations reads current balance from DataProgram
  5. Operations adds amount to balance
  6. Operations writes new balance to DataProgram
  7. Operations displays confirmation with new balance
- **Business Rules**: 
  - No explicit validation (accepts any numeric input within PIC constraints)
  - Zero amounts are technically allowed
  - Maximum amount: 999,999.99 (limited by PIC 9(6)V99)
- **Side Effects**: Increases account balance
- **Output**: Displays new balance after credit

#### 3. Debit Account Operation
- **Trigger**: User selects menu option 3
- **Process**:
  1. MainProgram calls Operations with 'DEBIT '
  2. Operations prompts user for amount
  3. User enters amount
  4. Operations reads current balance from DataProgram
  5. Operations checks if balance >= amount
  6. **If sufficient funds**:
     - Subtracts amount from balance
     - Writes new balance to DataProgram
     - Displays confirmation with new balance
  7. **If insufficient funds**:
     - Displays "Insufficient funds" message
     - Balance remains unchanged (no write operation)
- **Business Rules**:
  - **Overdraft Protection**: Cannot debit more than current balance
  - Zero amounts technically allowed
  - Maximum amount: 999,999.99
- **Side Effects**: Decreases account balance (only if sufficient funds)
- **Output**: Displays new balance or error message

#### 4. Exit Operation
- **Trigger**: User selects menu option 4
- **Process**:
  1. MainProgram sets CONTINUE-FLAG to 'NO'
  2. Loop terminates
  3. Goodbye message displayed
  4. Application stops
- **Business Rules**: None
- **Side Effects**: None (balance retained in memory until process terminates)
- **Output**: Goodbye message

### Transaction Characteristics

#### ACID Properties Analysis
- **Atomicity**: ⚠️ Partial - Each operation completes fully, but no rollback mechanism
- **Consistency**: ✅ Yes - Business rules enforced (insufficient funds check)
- **Isolation**: ⚠️ Not applicable - Single-user, single-threaded application
- **Durability**: ❌ No - Balance not persisted to file/database (memory only)

### Business Constraints
1. **Single Account**: Application manages only one account
2. **Single Currency**: No currency specification or conversion
3. **Decimal Precision**: Two decimal places (cents/paise)
4. **Balance Range**: 0.00 to 999,999.99
5. **Session-Based**: Balance resets on restart
6. **Sequential Processing**: One transaction at a time

---

## Data Structures

### WORKING-STORAGE Sections Overview

#### MainProgram Data Structure
```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
01  USER-CHOICE       PIC 9 VALUE 0.
01  CONTINUE-FLAG     PIC X(3) VALUE 'YES'.
```

| Field Name | Picture | Initial Value | Purpose | Valid Values |
|------------|---------|---------------|---------|--------------|
| USER-CHOICE | PIC 9 | 0 | Menu selection | 0-9 (validated 1-4) |
| CONTINUE-FLAG | PIC X(3) | 'YES' | Loop control | 'YES', 'NO' |

**Memory Layout:**
- Total size: 4 bytes
- USER-CHOICE: 1 byte numeric
- CONTINUE-FLAG: 3 bytes alphanumeric

#### Operations Data Structure
```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
01 OPERATION-TYPE     PIC X(6).
01 AMOUNT             PIC 9(6)V99.
01 FINAL-BALANCE      PIC 9(6)V99 VALUE 1000.00.

LINKAGE SECTION.
01 PASSED-OPERATION   PIC X(6).
```

| Field Name | Picture | Initial Value | Purpose | Range/Constraints |
|------------|---------|---------------|---------|-------------------|
| OPERATION-TYPE | PIC X(6) | None | Operation code | 'TOTAL ', 'CREDIT', 'DEBIT ' |
| AMOUNT | PIC 9(6)V99 | None | Transaction amount | 0.00 - 999,999.99 |
| FINAL-BALANCE | PIC 9(6)V99 | 1000.00 | Working balance | 0.00 - 999,999.99 |
| PASSED-OPERATION | PIC X(6) | Passed | Received operation | From MainProgram |

**Memory Layout:**
- Working-Storage: 20 bytes (6 + 8 + 8)
- Linkage Section: 6 bytes (passed by reference)

**Numeric Format Details:**
- `PIC 9(6)V99`: 6 digits before decimal, 2 after
- V (implied decimal point): Not stored, used for calculation alignment
- Examples: 1000.00, 999999.99, 0.01

#### DataProgram Data Structure
```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
01  STORAGE-BALANCE    PIC 9(6)V99 VALUE 1000.00.
01  OPERATION-TYPE     PIC X(6).

LINKAGE SECTION.
01  PASSED-OPERATION   PIC X(6).
01  BALANCE            PIC 9(6)V99.
```

| Field Name | Picture | Initial Value | Purpose | Persistence |
|------------|---------|---------------|---------|-------------|
| STORAGE-BALANCE | PIC 9(6)V99 | 1000.00 | Balance repository | Persists across calls |
| OPERATION-TYPE | PIC X(6) | None | Operation code | Per-call only |
| PASSED-OPERATION | PIC X(6) | Passed | Operation request | From Operations |
| BALANCE | PIC 9(6)V99 | Passed | Read/Write value | Bidirectional parameter |

**Memory Layout:**
- Working-Storage: 14 bytes (8 + 6) - Persists across calls
- Linkage Section: 14 bytes (6 + 8) - Parameters only

### Data Type Specifications

#### PICTURE Clause Breakdown

**PIC 9** (Single Numeric Digit)
- Size: 1 byte
- Range: 0-9
- Usage: Menu choice input
- Display format: Single digit

**PIC X(3)** (Alphanumeric String, 3 characters)
- Size: 3 bytes
- Content: Any characters
- Usage: Flag values ('YES', 'NO')
- Display format: Left-justified

**PIC X(6)** (Alphanumeric String, 6 characters)
- Size: 6 bytes
- Content: Any characters
- Usage: Operation codes
- Display format: Left-justified, space-padded
- Examples: 'TOTAL ', 'CREDIT', 'DEBIT '

**PIC 9(6)V99** (Decimal Number)
- Size: 8 bytes (internal representation)
- Integer part: 6 digits
- Decimal part: 2 digits
- V: Implied decimal point (not stored)
- Range: 0.00 to 999,999.99
- Usage: Monetary values, balances
- Display format: nnnnnn.nn

### Data Flow Between Programs

```
MainProgram             Operations              DataProgram
-----------             ----------              -----------
USER-CHOICE (9)    →    
CONTINUE-FLAG (X3)      
                        PASSED-OPERATION (X6) ←
                        OPERATION-TYPE (X6)
                        AMOUNT (9(6)V99)
                        FINAL-BALANCE (9(6)V99) 
                                           →    PASSED-OPERATION (X6)
                                           →    BALANCE (9(6)V99)
                                                 OPERATION-TYPE (X6)
                                                 STORAGE-BALANCE (9(6)V99)
                                           ←    BALANCE (9(6)V99)
                        FINAL-BALANCE (9(6)V99) ←
```

### Parameter Passing Mechanism

**CALL Statement Parameters:**
- All parameters passed **BY REFERENCE** (COBOL default)
- Caller and callee share same memory location
- Changes in callee reflect immediately in caller
- No data copying occurs

**Example:**
```cobol
CALL 'Operations' USING 'TOTAL '
```
- 'TOTAL ' is a literal constant (6 characters, space-padded)
- Passed by reference to Operations
- Received in PASSED-OPERATION parameter

```cobol
CALL 'DataProgram' USING 'READ', FINAL-BALANCE
```
- 'READ' is operation code literal
- FINAL-BALANCE is a variable passed by reference
- DataProgram can read from and write to FINAL-BALANCE

---

## File Interactions and Control Flow

### Call Graph

```
┌──────────────┐
│ MainProgram  │ (Entry Point)
└──────┬───────┘
       │
       │ CALL 'Operations' USING operation-code
       │
       ▼
┌──────────────┐
│  Operations  │
└──────┬───────┘
       │
       │ CALL 'DataProgram' USING operation, balance
       │
       ▼
┌──────────────┐
│ DataProgram  │
└──────────────┘
```

### Detailed Call Sequences

#### Sequence 1: View Balance (Option 1)

```
User → MainProgram → Operations → DataProgram → Operations → MainProgram → User

1. User enters "1"
2. MainProgram: CALL 'Operations' USING 'TOTAL '
3. Operations: MOVE 'TOTAL ' TO OPERATION-TYPE
4. Operations: CALL 'DataProgram' USING 'READ', FINAL-BALANCE
5. DataProgram: MOVE STORAGE-BALANCE TO BALANCE
6. DataProgram: GOBACK (returns to Operations)
7. Operations: DISPLAY "Current balance: " FINAL-BALANCE
8. Operations: GOBACK (returns to MainProgram)
9. MainProgram: Redisplays menu
```

**Data Flow:**
- MainProgram → Operations: 'TOTAL ' (operation code)
- Operations → DataProgram: 'READ' (operation), FINAL-BALANCE (output buffer)
- DataProgram → Operations: 1000.00 (via BALANCE parameter)
- Operations → User: "Current balance: 1000.00"

#### Sequence 2: Credit Account (Option 2)

```
User → MainProgram → Operations → User → Operations → DataProgram → Operations → 
DataProgram → Operations → MainProgram → User

1. User enters "2"
2. MainProgram: CALL 'Operations' USING 'CREDIT'
3. Operations: MOVE 'CREDIT' TO OPERATION-TYPE
4. Operations: DISPLAY "Enter credit amount: "
5. User enters amount (e.g., 500.00)
6. Operations: ACCEPT AMOUNT
7. Operations: CALL 'DataProgram' USING 'READ', FINAL-BALANCE
8. DataProgram: MOVE STORAGE-BALANCE TO BALANCE (returns 1000.00)
9. DataProgram: GOBACK
10. Operations: ADD AMOUNT TO FINAL-BALANCE (1000.00 + 500.00 = 1500.00)
11. Operations: CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
12. DataProgram: MOVE BALANCE TO STORAGE-BALANCE (stores 1500.00)
13. DataProgram: GOBACK
14. Operations: DISPLAY "Amount credited. New balance: " FINAL-BALANCE
15. Operations: GOBACK
16. MainProgram: Redisplays menu
```

**Data Flow:**
- MainProgram → Operations: 'CREDIT'
- User → Operations: 500.00 (amount)
- Operations → DataProgram: 'READ', FINAL-BALANCE
- DataProgram → Operations: 1000.00
- Operations (internal): 1000.00 + 500.00 = 1500.00
- Operations → DataProgram: 'WRITE', 1500.00
- DataProgram (internal): Updates STORAGE-BALANCE to 1500.00
- Operations → User: "Amount credited. New balance: 1500.00"

#### Sequence 3: Debit Account - Sufficient Funds (Option 3)

```
User → MainProgram → Operations → User → Operations → DataProgram → Operations → 
DataProgram → Operations → MainProgram → User

1. User enters "3"
2. MainProgram: CALL 'Operations' USING 'DEBIT '
3. Operations: MOVE 'DEBIT ' TO OPERATION-TYPE
4. Operations: DISPLAY "Enter debit amount: "
5. User enters amount (e.g., 300.00)
6. Operations: ACCEPT AMOUNT
7. Operations: CALL 'DataProgram' USING 'READ', FINAL-BALANCE
8. DataProgram: MOVE STORAGE-BALANCE TO BALANCE (returns 1500.00)
9. DataProgram: GOBACK
10. Operations: IF FINAL-BALANCE >= AMOUNT (1500.00 >= 300.00) TRUE
11. Operations: SUBTRACT AMOUNT FROM FINAL-BALANCE (1500.00 - 300.00 = 1200.00)
12. Operations: CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
13. DataProgram: MOVE BALANCE TO STORAGE-BALANCE (stores 1200.00)
14. DataProgram: GOBACK
15. Operations: DISPLAY "Amount debited. New balance: " FINAL-BALANCE
16. Operations: GOBACK
17. MainProgram: Redisplays menu
```

#### Sequence 4: Debit Account - Insufficient Funds (Option 3)

```
User → MainProgram → Operations → User → Operations → DataProgram → Operations → 
MainProgram → User

1. User enters "3"
2. MainProgram: CALL 'Operations' USING 'DEBIT '
3. Operations: MOVE 'DEBIT ' TO OPERATION-TYPE
4. Operations: DISPLAY "Enter debit amount: "
5. User enters amount (e.g., 5000.00)
6. Operations: ACCEPT AMOUNT
7. Operations: CALL 'DataProgram' USING 'READ', FINAL-BALANCE
8. DataProgram: MOVE STORAGE-BALANCE TO BALANCE (returns 1200.00)
9. DataProgram: GOBACK
10. Operations: IF FINAL-BALANCE >= AMOUNT (1200.00 >= 5000.00) FALSE
11. Operations: DISPLAY "Insufficient funds for this debit."
12. Operations: GOBACK (NO write to DataProgram - balance unchanged)
13. MainProgram: Redisplays menu
```

**Data Flow (Rejection Path):**
- MainProgram → Operations: 'DEBIT '
- User → Operations: 5000.00
- Operations → DataProgram: 'READ', FINAL-BALANCE
- DataProgram → Operations: 1200.00
- Operations (validation): 1200.00 < 5000.00 → REJECT
- Operations → User: "Insufficient funds for this debit."
- **Note**: No WRITE operation, STORAGE-BALANCE remains 1200.00

#### Sequence 5: Exit Application (Option 4)

```
User → MainProgram → (terminates)

1. User enters "4"
2. MainProgram: MOVE 'NO' TO CONTINUE-FLAG
3. MainProgram: Loop condition becomes false
4. MainProgram: DISPLAY "Exiting the program. Goodbye!"
5. MainProgram: STOP RUN
```

**Data Flow:**
- User → MainProgram: 4
- MainProgram (internal): Sets CONTINUE-FLAG = 'NO'
- MainProgram → User: "Exiting the program. Goodbye!"
- Program terminates

### Program State Transitions

```
[Start] 
   ↓
[Initialize MainProgram] 
   ↓ USER-CHOICE = 0, CONTINUE-FLAG = 'YES'
   ↓
[Display Menu] ←──────────────┐
   ↓                           │
[Accept Choice]                │
   ↓                           │
[Evaluate Choice]              │
   ├─ 1 → [Call Operations: VIEW]     → [Display Balance]    ──┤
   ├─ 2 → [Call Operations: CREDIT]   → [Update Balance]     ──┤
   ├─ 3 → [Call Operations: DEBIT]    → [Update/Reject]      ──┤
   ├─ 4 → [Set CONTINUE-FLAG = 'NO']  → [Exit Message] → [End]
   └─ Other → [Display Error]         ───────────────────────┘
```

### Control Transfer Mechanisms

#### CALL Statement
- **Syntax**: `CALL 'ProgramName' USING parameter1, parameter2`
- **Behavior**: 
  - Transfers control to called program
  - Passes parameters by reference
  - Called program executes
  - Control returns via GOBACK or EXIT PROGRAM

#### GOBACK Statement
- **Purpose**: Return control to calling program
- **Usage**: At end of called programs (Operations, DataProgram)
- **Effect**: Resumes execution in caller at statement after CALL

#### STOP RUN Statement
- **Purpose**: Terminate entire application
- **Usage**: In MainProgram when user exits
- **Effect**: Ends all program execution, closes files, releases resources

#### PERFORM UNTIL Loop
- **Purpose**: Iterative execution
- **Condition**: `CONTINUE-FLAG = 'NO'`
- **Behavior**: 
  - Executes loop body
  - Checks condition
  - Repeats until condition is true
  - Exits loop and continues

---

## Business Rules and Validation Logic

### Input Validation

#### Menu Choice Validation (MainProgram)
```cobol
EVALUATE USER-CHOICE
    WHEN 1
    WHEN 2
    WHEN 3
    WHEN 4
        (valid operations)
    WHEN OTHER
        DISPLAY "Invalid choice, please select 1-4."
END-EVALUATE
```

**Rule**: User choice must be 1, 2, 3, or 4
- **Valid inputs**: 1-4
- **Invalid inputs**: 0, 5-9, non-numeric characters
- **Error handling**: Display error message, redisplay menu
- **Recovery**: User can immediately try again

#### Amount Input Validation
**Current State**: ⚠️ **No explicit validation**

**Implicit Constraints**:
- COBOL ACCEPT statement with PIC 9(6)V99
- Allows: 0.00 to 999,999.99
- Non-numeric input causes runtime error or unexpected behavior

**Missing Validations**:
- No check for negative amounts (implicit in PICTURE clause)
- No check for zero amounts
- No minimum transaction amount
- No maximum transaction limit (beyond PIC constraint)

### Business Rules

#### Rule 1: Insufficient Funds Protection
**Location**: operations.cob, DEBIT operation

```cobol
IF FINAL-BALANCE >= AMOUNT
    (allow debit)
ELSE
    DISPLAY "Insufficient funds for this debit."
END-IF
```

**Rule Definition**: A debit transaction can only proceed if the current balance is greater than or equal to the debit amount.

**Enforcement**: 
- ✅ Strictly enforced before any balance modification
- ✅ No overdrafts allowed
- ✅ Balance remains unchanged on rejection

**Edge Cases**:
- **Balance = Amount**: Allowed (balance becomes 0.00)
- **Balance = 0.00, Amount > 0**: Rejected
- **Amount = 0.00**: Allowed (but balance unchanged)

#### Rule 2: Starting Balance
**Location**: data.cob, STORAGE-BALANCE initialization

```cobol
01  STORAGE-BALANCE    PIC 9(6)V99 VALUE 1000.00.
```

**Rule Definition**: Every new session starts with a balance of 1000.00

**Enforcement**:
- ✅ Set at program initialization
- ✅ Cannot be configured or changed
- ⚠️ Resets to 1000.00 on every application restart

#### Rule 3: Balance Bounds
**Implicit Rule**: Balance must be between 0.00 and 999,999.99

**Constraints**:
- **Lower bound**: 0.00 (enforced by insufficient funds rule)
- **Upper bound**: 999,999.99 (enforced by PIC 9(6)V99)
- **Overflow scenario**: Crediting beyond max causes data truncation or error

**Example Overflow**:
```
Current balance: 999,500.00
Credit amount: 1,000.00
Result: Data error (exceeds PIC 9(6)V99 capacity)
```

#### Rule 4: Transaction Atomicity
**Implicit Rule**: Each transaction must complete fully or not at all

**Read-Modify-Write Pattern**:
```
1. READ current balance
2. Perform calculation
3. WRITE new balance
```

**Enforcement**:
- ✅ All three steps execute in sequence
- ⚠️ No rollback mechanism on failure
- ⚠️ No transaction logging

**Risk**: If program crashes between READ and WRITE, balance may be inconsistent

#### Rule 5: Single Session State
**Rule Definition**: Balance persists only within a single application session

**Implications**:
- ✅ Multiple operations in same session maintain updated balance
- ❌ Balance not saved to disk/file
- ❌ No balance history or audit trail
- ❌ No recovery from system crash

#### Rule 6: Operation Code Format
**Rule Definition**: Operation codes must be exactly 6 characters

**Examples**:
- 'TOTAL ' (5 chars + 1 space)
- 'CREDIT' (6 chars, no space)
- 'DEBIT ' (5 chars + 1 space)
- 'READ  ' (4 chars + 2 spaces) - for DataProgram
- 'WRITE ' (5 chars + 1 space) - for DataProgram

**Enforcement**: String comparison in IF statements requires exact match

### Validation Summary Table

| Validation Type | Implemented | Location | Strength |
|-----------------|-------------|----------|----------|
| Menu choice (1-4) | ✅ Yes | MainProgram | Strong |
| Numeric amount | ⚠️ Partial | PIC clause | Weak |
| Negative amount | ⚠️ Implicit | PIC 9 (unsigned) | Medium |
| Zero amount | ❌ No | None | None |
| Minimum transaction | ❌ No | None | None |
| Maximum transaction | ⚠️ Implicit | PIC 9(6)V99 | Weak |
| Insufficient funds | ✅ Yes | Operations, DEBIT | Strong |
| Balance overflow | ❌ No | None | None |
| Operation code | ⚠️ Implicit | String match | Medium |

### Error Handling

#### Error Types and Responses

**Type 1: Invalid Menu Choice**
- **Error**: User enters invalid menu option
- **Detection**: EVALUATE WHEN OTHER
- **Response**: Display error message
- **Recovery**: Redisplay menu, allow retry
- **Data Impact**: None

**Type 2: Insufficient Funds**
- **Error**: Debit amount exceeds balance
- **Detection**: IF FINAL-BALANCE >= AMOUNT
- **Response**: Display "Insufficient funds" message
- **Recovery**: Balance unchanged, return to menu
- **Data Impact**: None (protected)

**Type 3: Invalid Amount Input**
- **Error**: Non-numeric input for amount
- **Detection**: ACCEPT statement runtime check
- **Response**: COBOL runtime error (program may crash)
- **Recovery**: None (program termination likely)
- **Data Impact**: Potentially inconsistent state

**Type 4: Balance Overflow**
- **Error**: Credit exceeds PIC 9(6)V99 capacity
- **Detection**: None (implicit COBOL behavior)
- **Response**: Data truncation or runtime error
- **Recovery**: None
- **Data Impact**: Balance corruption

#### Missing Error Handling

1. **No try-catch equivalent**: COBOL programs lack exception handling
2. **No input sanitization**: ACCEPT statements trust user input
3. **No overflow detection**: Arithmetic operations don't check bounds
4. **No logging**: Errors not recorded for debugging
5. **No graceful degradation**: Runtime errors cause crashes

---

## Execution Flow Map

### Complete Application Flow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                     APPLICATION START                        │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
              ┌──────────────────────────────┐
              │  Initialize MainProgram      │
              │  - USER-CHOICE = 0           │
              │  - CONTINUE-FLAG = 'YES'     │
              └──────────────┬───────────────┘
                             │
                             ▼
              ┌──────────────────────────────┐
              │  Initialize Operations       │
              │  - FINAL-BALANCE = 1000.00   │
              └──────────────┬───────────────┘
                             │
                             ▼
              ┌──────────────────────────────┐
              │  Initialize DataProgram      │
              │  - STORAGE-BALANCE = 1000.00 │
              └──────────────┬───────────────┘
                             │
┌────────────────────────────┼────────────────────────────────┐
│                    MAIN PROGRAM LOOP                         │
│                            ▼                                 │
│            ┌───────────────────────────┐                     │
│            │   Display Menu            │                     │
│            │   ----------------------  │                     │
│            │   1. View Balance         │                     │
│            │   2. Credit Account       │                     │
│            │   3. Debit Account        │                     │
│            │   4. Exit                 │                     │
│            └─────────────┬─────────────┘                     │
│                          │                                   │
│                          ▼                                   │
│            ┌───────────────────────────┐                     │
│            │   Accept USER-CHOICE      │                     │
│            └─────────────┬─────────────┘                     │
│                          │                                   │
│                          ▼                                   │
│            ┌───────────────────────────┐                     │
│            │   Evaluate USER-CHOICE    │                     │
│            └─────────────┬─────────────┘                     │
│                          │                                   │
│         ┌────────────────┼────────────────┬─────────────┐    │
│         │                │                │             │    │
│         ▼                ▼                ▼             ▼    │
│    ┌────────┐      ┌─────────┐     ┌─────────┐   ┌────────┐│
│    │ WHEN 1 │      │ WHEN 2  │     │ WHEN 3  │   │ WHEN 4 ││
│    │ (VIEW) │      │ (CREDIT)│     │ (DEBIT) │   │ (EXIT) ││
│    └───┬────┘      └────┬────┘     └────┬────┘   └───┬────┘│
│        │                │               │             │     │
│        ▼                ▼               ▼             │     │
│ ┌──────────────────────────────────────────────┐     │     │
│ │         CALL Operations Program              │     │     │
│ └──────────────┬───────────────────────────────┘     │     │
│                │                                      │     │
└────────────────┼──────────────────────────────────────┼─────┘
                 │                                      │
                 ▼                                      │
┌──────────────────────────────────────────────────┐    │
│            OPERATIONS PROGRAM LOGIC              │    │
└──────────────┬───────────────────────────────────┘    │
               │                                        │
               ▼                                        │
       ┌──────────────────┐                             │
       │ Operation Type?  │                             │
       └────┬─────────────┘                             │
            │                                           │
    ┌───────┼────────┬──────────────┐                  │
    │       │        │              │                  │
    ▼       ▼        ▼              ▼                  │
┌───────┐ ┌──────┐ ┌──────┐     ┌──────┐              │
│ TOTAL │ │CREDIT│ │DEBIT │     │OTHER │              │
└───┬───┘ └──┬───┘ └──┬───┘     └──────┘              │
    │        │        │                                │
    │        │        │                                │
    ▼        ▼        ▼                                │
    │    ┌───────────────────┐                         │
    │    │ Prompt for AMOUNT │                         │
    │    │ Accept AMOUNT     │                         │
    │    └───────┬───────────┘                         │
    │            │                                     │
    │            ▼                                     │
    │    ┌───────────────────────────────┐             │
    │    │  CALL DataProgram: READ       │             │
    └────┤  (Get current balance)        │             │
         └───────┬───────────────────────┘             │
                 │                                     │
         ┌───────┴────────┐                            │
         │                │                            │
         ▼                ▼                            │
    [CREDIT PATH]    [DEBIT PATH]                      │
         │                │                            │
         │                ▼                            │
         │        ┌──────────────────┐                 │
         │        │ BALANCE >= AMOUNT│                 │
         │        └────┬────────┬────┘                 │
         │             │        │                      │
         │            YES      NO                      │
         │             │        │                      │
         ▼             ▼        ▼                      │
    ┌────────┐   ┌────────┐ ┌──────────────────┐      │
    │ADD     │   │SUBTRACT│ │Display           │      │
    │AMOUNT  │   │AMOUNT  │ │"Insufficient     │      │
    └───┬────┘   └───┬────┘ │ funds"           │      │
        │            │       └────────┬─────────┘      │
        │            │                │                │
        ▼            ▼                │                │
    ┌──────────────────────┐          │                │
    │ CALL DataProgram:    │          │                │
    │ WRITE (Save balance) │          │                │
    └──────┬───────────────┘          │                │
           │                          │                │
           ▼                          │                │
    ┌──────────────────────┐          │                │
    │ Display confirmation │          │                │
    │ with new balance     │◄─────────┘                │
    └──────┬───────────────┘                           │
           │                                           │
           ▼                                           │
    ┌──────────────────────┐                           │
    │ GOBACK to MainProgram│                           │
    └──────┬───────────────┘                           │
           │                                           │
           │                                           │
           └───────────────┬───────────────────────────┘
                           │
                           ▼
                ┌──────────────────────┐
                │ Set CONTINUE-FLAG    │
                │ to 'NO'              │
                └──────┬───────────────┘
                       │
                       ▼
                ┌──────────────────────┐
                │ Display             │
                │ "Exiting... Goodbye!"│
                └──────┬───────────────┘
                       │
                       ▼
                ┌──────────────────────┐
                │    STOP RUN          │
                └──────┬───────────────┘
                       │
                       ▼
              ┌────────────────────┐
              │  APPLICATION END   │
              └────────────────────┘
```

### State Machine View

```
States:
  [MENU]    - Displaying menu, waiting for input
  [VIEW]    - Viewing balance
  [CREDIT]  - Processing credit
  [DEBIT]   - Processing debit
  [EXIT]    - Terminating application
  [ERROR]   - Handling invalid input

Transitions:
  MENU --[choice=1]--> VIEW --[display]--> MENU
  MENU --[choice=2]--> CREDIT --[success]--> MENU
  MENU --[choice=3]--> DEBIT --[success/fail]--> MENU
  MENU --[choice=4]--> EXIT --[cleanup]--> (terminate)
  MENU --[invalid]--> ERROR --[message]--> MENU
```

### Timing Diagram

```
Time  MainProgram    Operations       DataProgram      User
│     
├─0s  ┌──────┐       
│     │Start │       
│     └──┬───┘       
│        │           
├─1s  ┌──▼─────┐     
│     │Display │     
│     │Menu    │     
│     └──┬─────┘     
│        │           
├─2s     │                                          ┌───────┐
│        │◄─────────────────────────────────────────│Enter 2│
│        │                                          └───────┘
│        │           
├─3s  ┌──▼──────────────┐
│     │CALL Operations  │
│     │USING 'CREDIT'   │
│     └────────┬────────┘
│              │         
├─4s           │        ┌──────────────┐
│              ├────────►Accept        │
│              │        │PASSED-OP     │
│              │        └──┬───────────┘
│              │           │
├─5s           │        ┌──▼──────────┐
│              │        │Display      │
│              │        │"Enter amt"  │
│              │        └──┬──────────┘
│              │           │                       ┌────────┐
├─6s           │           │◄──────────────────────│Enter   │
│              │           │                       │500.00  │
│              │           │                       └────────┘
│              │        ┌──▼──────────┐
├─7s           │        │ACCEPT AMOUNT│
│              │        └──┬──────────┘
│              │           │
│              │        ┌──▼──────────────────┐
├─8s           │        │CALL DataProgram     │
│              │        │USING 'READ',BALANCE │
│              │        └──┬──────────────────┘
│              │           │          ┌──────────────┐
├─9s           │           ├──────────►Read         │
│              │           │          │STORAGE-BAL   │
│              │           │          └──┬───────────┘
│              │           │             │
├─10s          │           │          ┌──▼───────────┐
│              │           │◄─────────┤MOVE to       │
│              │           │          │BALANCE(1000) │
│              │           │          │GOBACK        │
│              │           │          └──────────────┘
│              │        ┌──▼──────────┐
├─11s          │        │ADD AMOUNT to│
│              │        │FINAL-BALANCE│
│              │        │(1500.00)    │
│              │        └──┬──────────┘
│              │           │
│              │        ┌──▼──────────────────┐
├─12s          │        │CALL DataProgram     │
│              │        │USING 'WRITE',BAL    │
│              │        └──┬──────────────────┘
│              │           │          ┌──────────────┐
├─13s          │           ├──────────►Write        │
│              │           │          │STORAGE-BAL   │
│              │           │          │(1500.00)     │
│              │           │          └──┬───────────┘
│              │           │             │
├─14s          │           │          ┌──▼───────────┐
│              │           │◄─────────┤GOBACK        │
│              │           │          └──────────────┘
│              │        ┌──▼──────────┐
├─15s          │        │Display      │
│              │        │"Credited... │                ┌────────┐
│              │        │1500.00"     ├────────────────►Screen │
│              │        └──┬──────────┘                └────────┘
│              │           │
├─16s          │        ┌──▼──────────┐
│              │◄───────┤GOBACK       │
│              │        └─────────────┘
├─17s  ┌───▼──────┐
│      │Redisplay │
│      │Menu      │
│      └──────────┘
```

### Memory State Evolution

```
Operation Sequence: Credit 500.00

Time  Operation           MainProgram        Operations           DataProgram
                          CONTINUE-FLAG      FINAL-BALANCE        STORAGE-BALANCE
────  ─────────────────   ───────────────    ────────────────     ───────────────
T0    Start               'YES'              1000.00              1000.00
T1    Display Menu        'YES'              1000.00              1000.00
T2    User enters 2       'YES'              1000.00              1000.00
T3    Call Operations     'YES'              1000.00              1000.00
T4    Accept operation    'YES'              1000.00              1000.00
T5    Prompt for amount   'YES'              1000.00              1000.00
T6    Accept 500.00       'YES'              1000.00              1000.00
T7    Call READ           'YES'              1000.00              1000.00
T8    Read balance        'YES'              1000.00 ← 1000.00    1000.00
T9    Add 500.00          'YES'              1500.00              1000.00
T10   Call WRITE          'YES'              1500.00 → 1500.00    1000.00
T11   Write balance       'YES'              1500.00              1500.00 ✓
T12   Display result      'YES'              1500.00              1500.00
T13   Return to Main      'YES'              1500.00              1500.00
T14   Menu redisplayed    'YES'              1500.00              1500.00

Legend:
  ← : Data read from DataProgram
  → : Data sent to DataProgram
  ✓ : Data persisted
```

---

## Appendix

### COBOL Language Reference

#### Key COBOL Statements Used

**IDENTIFICATION DIVISION**
- Identifies the program name
- Required first division

**DATA DIVISION**
- Defines data structures
- Two sections: WORKING-STORAGE and LINKAGE

**WORKING-STORAGE SECTION**
- Internal variables
- Persist across calls within session
- Initialized at program load

**LINKAGE SECTION**
- Parameters from calling program
- Passed by reference
- Not allocated by this program

**PROCEDURE DIVISION**
- Contains executable code
- Logic and control flow

**DISPLAY**
- Output to console
- Syntax: `DISPLAY "text" variable`

**ACCEPT**
- Input from console
- Syntax: `ACCEPT variable-name`

**CALL**
- Invoke subprogram
- Syntax: `CALL 'ProgramName' USING param1, param2`

**EVALUATE**
- Multi-way conditional (like switch/case)
- Syntax: `EVALUATE variable WHEN value ... END-EVALUATE`

**IF/ELSE**
- Conditional execution
- Syntax: `IF condition ... ELSE ... END-IF`

**PERFORM UNTIL**
- Loop structure
- Syntax: `PERFORM UNTIL condition ... END-PERFORM`

**ADD/SUBTRACT**
- Arithmetic operations
- Syntax: `ADD value TO variable`

**MOVE**
- Assignment
- Syntax: `MOVE source TO destination`

**GOBACK**
- Return to caller
- Used in called programs

**STOP RUN**
- Terminate application
- Used in main program

### Picture Clause Reference

| Picture | Description | Size | Range | Example |
|---------|-------------|------|-------|---------|
| PIC 9 | Single digit | 1 byte | 0-9 | 5 |
| PIC 9(n) | n digits | n bytes | 0 to 10^n-1 | 123 (n=3) |
| PIC 9(n)V99 | Decimal | n+2 bytes | 0.00 to max | 1234.56 |
| PIC X | Single char | 1 byte | Any | 'A' |
| PIC X(n) | String | n bytes | Any | 'HELLO' |

### Common Acronyms

- **COBOL**: Common Business-Oriented Language
- **PIC**: Picture (data type specification)
- **V**: Virtual decimal point (implied, not stored)
- **9**: Numeric digit (0-9)
- **X**: Alphanumeric character (any)

### Program Compilation Command

```bash
cobc -x main.cob operations.cob data.cob -o accountsystem
```

**Flags:**
- `-x`: Create executable
- `-o`: Output file name

### Program Execution

```bash
./accountsystem
```

---

## Summary and Key Takeaways

### Application Characteristics

✅ **Strengths:**
- Clear separation of concerns (3-tier architecture)
- Modular design (easy to understand each file)
- User-friendly menu interface
- Insufficient funds protection
- Consistent transaction pattern

⚠️ **Limitations:**
- No data persistence (memory only)
- No transaction logging or audit trail
- Limited error handling
- Single account only
- No user authentication
- No concurrent access support
- Balance resets on restart

### Critical Business Logic Points

1. **Starting Balance**: Always 1000.00 on new session
2. **Overdraft Protection**: Cannot debit more than balance
3. **Transaction Pattern**: Read → Calculate → Write
4. **Balance Range**: 0.00 to 999,999.99
5. **Session-Based**: State lost on exit

### Modernization Considerations

When converting to Node.js, preserve:
- ✅ Insufficient funds validation
- ✅ Read-Modify-Write transaction pattern
- ✅ User interface flow
- ✅ Balance initialization logic
- ✅ Error messages and feedback

Consider adding:
- 💡 Data persistence (database or file)
- 💡 Input validation (min/max amounts, zero checks)
- 💡 Transaction history/logging
- 💡 Multiple accounts support
- 💡 User authentication
- 💡 Error logging and recovery
- 💡 Unit and integration tests

### File Dependencies Summary

```
main.cob
  └─ Calls: operations.cob
             └─ Calls: data.cob
```

**No circular dependencies** - Clean unidirectional call hierarchy

---

*This documentation was created to support the modernization of the COBOL application to Node.js. It captures the current business logic, architecture, and behavior to ensure no functionality is lost during conversion.*

**Document Version**: 1.0  
**Last Updated**: 2025-11-05  
**Application Version**: Legacy COBOL Account Management System  
**Purpose**: Phase 1 Analysis for Modernization Project
