# 📊 System Architecture and Data Flow Diagrams

This document contains visual diagrams showing the architecture, data flow, and component interactions of the COBOL Account Management System. All diagrams use Mermaid format for GitHub rendering.

---

## Table of Contents

1. [High-Level System Architecture](#1-high-level-system-architecture)
2. [Component Interaction Diagram](#2-component-interaction-diagram)
3. [Detailed Sequence Diagrams](#3-detailed-sequence-diagrams)
   - [View Balance Flow](#view-balance-flow)
   - [Credit Account Flow](#credit-account-flow)
   - [Debit Account Flow (Sufficient Funds)](#debit-account-flow-sufficient-funds)
   - [Debit Account Flow (Insufficient Funds)](#debit-account-flow-insufficient-funds)
   - [Exit Application Flow](#exit-application-flow)
4. [Data Flow Diagram](#4-data-flow-diagram)
5. [State Machine Diagram](#5-state-machine-diagram)
6. [Module Dependency Graph](#6-module-dependency-graph)

---

## 1. High-Level System Architecture

This diagram shows the three-tier layered architecture of the COBOL application.

```mermaid
graph TB
    subgraph "Presentation Layer"
        A[Main Program<br/>main.cob]
    end
    
    subgraph "Business Logic Layer"
        B[Operations<br/>operations.cob]
    end
    
    subgraph "Data Access Layer"
        C[Data Program<br/>data.cob]
    end
    
    subgraph "External Actor"
        D[User]
    end
    
    D -->|Menu Selection| A
    A -->|Display Menu| D
    A -->|CALL 'Operations'<br/>USING operation-code| B
    B -->|CALL 'DataProgram'<br/>USING operation, balance| C
    C -->|Return Balance| B
    B -->|Display Results| D
    
    style A fill:#e1f5ff,stroke:#0277bd,stroke-width:2px
    style B fill:#fff4e1,stroke:#f57c00,stroke-width:2px
    style C fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style D fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
```

**Legend:**
- **Presentation Layer (Blue)**: User interface and menu management
- **Business Logic Layer (Orange)**: Transaction processing and business rules
- **Data Access Layer (Green)**: Data storage and retrieval
- **External Actor (Purple)**: End user interaction

---

## 2. Component Interaction Diagram

This diagram shows the detailed component structure and their relationships.

```mermaid
graph LR
    subgraph "MainProgram Components"
        MP[Main Logic]
        UC[USER-CHOICE<br/>PIC 9]
        CF[CONTINUE-FLAG<br/>PIC X(3)]
    end
    
    subgraph "Operations Components"
        OP[Operation Logic]
        OT[OPERATION-TYPE<br/>PIC X(6)]
        AM[AMOUNT<br/>PIC 9(6)V99]
        FB[FINAL-BALANCE<br/>PIC 9(6)V99]
    end
    
    subgraph "DataProgram Components"
        DP[Data Access Logic]
        SB[STORAGE-BALANCE<br/>PIC 9(6)V99]
        DT[OPERATION-TYPE<br/>PIC X(6)]
    end
    
    MP -->|CALL with<br/>'TOTAL'/'CREDIT'/'DEBIT'| OP
    OP -->|CALL with<br/>'READ'/'WRITE'| DP
    UC -.->|Controls| MP
    CF -.->|Controls| MP
    OT -.->|Determines| OP
    AM -.->|Input to| OP
    FB -.->|Working storage| OP
    SB -.->|Persistent state| DP
    DT -.->|Determines| DP
    
    style MP fill:#e1f5ff,stroke:#0277bd,stroke-width:2px
    style OP fill:#fff4e1,stroke:#f57c00,stroke-width:2px
    style DP fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style UC fill:#ffebee,stroke:#c62828,stroke-width:1px
    style CF fill:#ffebee,stroke:#c62828,stroke-width:1px
    style OT fill:#fff9c4,stroke:#f57f17,stroke-width:1px
    style AM fill:#fff9c4,stroke:#f57f17,stroke-width:1px
    style FB fill:#fff9c4,stroke:#f57f17,stroke-width:1px
    style SB fill:#c8e6c9,stroke:#2e7d32,stroke-width:1px
    style DT fill:#c8e6c9,stroke:#2e7d32,stroke-width:1px
```

**Legend:**
- **Solid arrows**: Program calls
- **Dotted arrows**: Data dependencies
- **Blue boxes**: Presentation layer components
- **Orange boxes**: Business logic components
- **Green boxes**: Data layer components
- **Light red boxes**: Control variables
- **Light yellow boxes**: Business data variables
- **Light green boxes**: Storage variables

---

## 3. Detailed Sequence Diagrams

### View Balance Flow

This sequence diagram shows the complete flow when a user views their account balance.

```mermaid
sequenceDiagram
    actor User
    participant MainProgram
    participant Operations
    participant DataProgram
    
    Note over User,DataProgram: User selects option 1 (View Balance)
    
    User->>MainProgram: Enter choice: 1
    activate MainProgram
    MainProgram->>MainProgram: EVALUATE USER-CHOICE = 1
    MainProgram->>Operations: CALL 'Operations' USING 'TOTAL '
    activate Operations
    
    Operations->>Operations: MOVE 'TOTAL ' TO OPERATION-TYPE
    Operations->>Operations: IF OPERATION-TYPE = 'TOTAL '
    
    Operations->>DataProgram: CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    activate DataProgram
    DataProgram->>DataProgram: MOVE PASSED-OPERATION TO OPERATION-TYPE
    DataProgram->>DataProgram: IF OPERATION-TYPE = 'READ'
    DataProgram->>DataProgram: MOVE STORAGE-BALANCE TO BALANCE
    DataProgram-->>Operations: Return (FINAL-BALANCE = 1000.00)
    deactivate DataProgram
    
    Operations->>User: DISPLAY "Current balance: 1000.00"
    Operations->>Operations: GOBACK
    deactivate Operations
    
    MainProgram->>User: Display Menu (loop continues)
    deactivate MainProgram
```

---

### Credit Account Flow

This sequence diagram shows the complete flow when a user credits their account.

```mermaid
sequenceDiagram
    actor User
    participant MainProgram
    participant Operations
    participant DataProgram
    
    Note over User,DataProgram: User selects option 2 (Credit Account)
    
    User->>MainProgram: Enter choice: 2
    activate MainProgram
    MainProgram->>MainProgram: EVALUATE USER-CHOICE = 2
    MainProgram->>Operations: CALL 'Operations' USING 'CREDIT'
    activate Operations
    
    Operations->>Operations: MOVE 'CREDIT' TO OPERATION-TYPE
    Operations->>Operations: IF OPERATION-TYPE = 'CREDIT'
    Operations->>User: DISPLAY "Enter credit amount: "
    User->>Operations: Input: 500.00
    Operations->>Operations: ACCEPT AMOUNT (500.00)
    
    Note over Operations,DataProgram: Read current balance
    Operations->>DataProgram: CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    activate DataProgram
    DataProgram->>DataProgram: MOVE STORAGE-BALANCE TO BALANCE
    DataProgram-->>Operations: Return (FINAL-BALANCE = 1000.00)
    deactivate DataProgram
    
    Note over Operations: Perform calculation
    Operations->>Operations: ADD AMOUNT TO FINAL-BALANCE<br/>(1000.00 + 500.00 = 1500.00)
    
    Note over Operations,DataProgram: Write updated balance
    Operations->>DataProgram: CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
    activate DataProgram
    DataProgram->>DataProgram: MOVE BALANCE TO STORAGE-BALANCE
    DataProgram->>DataProgram: STORAGE-BALANCE = 1500.00
    DataProgram-->>Operations: Return
    deactivate DataProgram
    
    Operations->>User: DISPLAY "Amount credited. New balance: 1500.00"
    Operations->>Operations: GOBACK
    deactivate Operations
    
    MainProgram->>User: Display Menu (loop continues)
    deactivate MainProgram
```

---

### Debit Account Flow (Sufficient Funds)

This sequence diagram shows the flow when a user debits their account with sufficient funds.

```mermaid
sequenceDiagram
    actor User
    participant MainProgram
    participant Operations
    participant DataProgram
    
    Note over User,DataProgram: User selects option 3 (Debit Account)
    
    User->>MainProgram: Enter choice: 3
    activate MainProgram
    MainProgram->>MainProgram: EVALUATE USER-CHOICE = 3
    MainProgram->>Operations: CALL 'Operations' USING 'DEBIT '
    activate Operations
    
    Operations->>Operations: MOVE 'DEBIT ' TO OPERATION-TYPE
    Operations->>Operations: IF OPERATION-TYPE = 'DEBIT '
    Operations->>User: DISPLAY "Enter debit amount: "
    User->>Operations: Input: 300.00
    Operations->>Operations: ACCEPT AMOUNT (300.00)
    
    Note over Operations,DataProgram: Read current balance
    Operations->>DataProgram: CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    activate DataProgram
    DataProgram->>DataProgram: MOVE STORAGE-BALANCE TO BALANCE
    DataProgram-->>Operations: Return (FINAL-BALANCE = 1500.00)
    deactivate DataProgram
    
    Note over Operations: Validate sufficient funds
    Operations->>Operations: IF FINAL-BALANCE >= AMOUNT<br/>(1500.00 >= 300.00) = TRUE
    
    Note over Operations: Perform calculation
    Operations->>Operations: SUBTRACT AMOUNT FROM FINAL-BALANCE<br/>(1500.00 - 300.00 = 1200.00)
    
    Note over Operations,DataProgram: Write updated balance
    Operations->>DataProgram: CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
    activate DataProgram
    DataProgram->>DataProgram: MOVE BALANCE TO STORAGE-BALANCE
    DataProgram->>DataProgram: STORAGE-BALANCE = 1200.00
    DataProgram-->>Operations: Return
    deactivate DataProgram
    
    Operations->>User: DISPLAY "Amount debited. New balance: 1200.00"
    Operations->>Operations: GOBACK
    deactivate Operations
    
    MainProgram->>User: Display Menu (loop continues)
    deactivate MainProgram
```

---

### Debit Account Flow (Insufficient Funds)

This sequence diagram shows the flow when a user attempts to debit more than their balance.

```mermaid
sequenceDiagram
    actor User
    participant MainProgram
    participant Operations
    participant DataProgram
    
    Note over User,DataProgram: User selects option 3 (Debit Account)
    
    User->>MainProgram: Enter choice: 3
    activate MainProgram
    MainProgram->>MainProgram: EVALUATE USER-CHOICE = 3
    MainProgram->>Operations: CALL 'Operations' USING 'DEBIT '
    activate Operations
    
    Operations->>Operations: MOVE 'DEBIT ' TO OPERATION-TYPE
    Operations->>Operations: IF OPERATION-TYPE = 'DEBIT '
    Operations->>User: DISPLAY "Enter debit amount: "
    User->>Operations: Input: 5000.00
    Operations->>Operations: ACCEPT AMOUNT (5000.00)
    
    Note over Operations,DataProgram: Read current balance
    Operations->>DataProgram: CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    activate DataProgram
    DataProgram->>DataProgram: MOVE STORAGE-BALANCE TO BALANCE
    DataProgram-->>Operations: Return (FINAL-BALANCE = 1200.00)
    deactivate DataProgram
    
    Note over Operations: Validate sufficient funds
    Operations->>Operations: IF FINAL-BALANCE >= AMOUNT<br/>(1200.00 >= 5000.00) = FALSE
    
    Note over Operations: Reject transaction - No write operation
    Operations->>User: DISPLAY "Insufficient funds for this debit."
    
    Note over Operations,DataProgram: Balance remains unchanged at 1200.00
    
    Operations->>Operations: GOBACK
    deactivate Operations
    
    MainProgram->>User: Display Menu (loop continues)
    deactivate MainProgram
```

---

### Exit Application Flow

This sequence diagram shows the flow when a user exits the application.

```mermaid
sequenceDiagram
    actor User
    participant MainProgram
    
    Note over User,MainProgram: User selects option 4 (Exit)
    
    User->>MainProgram: Enter choice: 4
    activate MainProgram
    MainProgram->>MainProgram: EVALUATE USER-CHOICE = 4
    MainProgram->>MainProgram: MOVE 'NO' TO CONTINUE-FLAG
    MainProgram->>MainProgram: Loop condition = FALSE<br/>(CONTINUE-FLAG = 'NO')
    MainProgram->>MainProgram: Exit PERFORM UNTIL loop
    MainProgram->>User: DISPLAY "Exiting the program. Goodbye!"
    MainProgram->>MainProgram: STOP RUN
    deactivate MainProgram
    
    Note over User,MainProgram: Application terminates
```

---

## 4. Data Flow Diagram

This diagram illustrates how data enters, transforms, and flows through the system.

```mermaid
graph TB
    Start([User Input]) --> Input[User Choice<br/>1-4]
    
    Input --> Decision{Route Based<br/>on Choice}
    
    Decision -->|1| ViewOp[View Balance<br/>Operation]
    Decision -->|2| CreditOp[Credit Account<br/>Operation]
    Decision -->|3| DebitOp[Debit Account<br/>Operation]
    Decision -->|4| ExitOp[Exit Application]
    
    ViewOp --> ReadData[Read Balance<br/>from Storage]
    CreditOp --> InputCredit[Accept Credit<br/>Amount Input]
    DebitOp --> InputDebit[Accept Debit<br/>Amount Input]
    
    InputCredit --> ReadCr[Read Current<br/>Balance]
    InputDebit --> ReadDb[Read Current<br/>Balance]
    
    ReadCr --> CalcCr[Calculate New Balance:<br/>Balance + Amount]
    ReadDb --> ValidateDb{Validate:<br/>Balance >= Amount?}
    
    ValidateDb -->|Yes| CalcDb[Calculate New Balance:<br/>Balance - Amount]
    ValidateDb -->|No| ErrorMsg[Display Error:<br/>Insufficient Funds]
    
    CalcCr --> WriteData1[Write Updated<br/>Balance to Storage]
    CalcDb --> WriteData2[Write Updated<br/>Balance to Storage]
    
    ReadData --> Display1[Display Current<br/>Balance]
    WriteData1 --> Display2[Display New<br/>Balance]
    WriteData2 --> Display3[Display New<br/>Balance]
    ErrorMsg --> ReturnMenu1[Return to Menu]
    
    Display1 --> ReturnMenu2[Return to Menu]
    Display2 --> ReturnMenu3[Return to Menu]
    Display3 --> ReturnMenu4[Return to Menu]
    
    ReturnMenu1 --> Loop{Continue?}
    ReturnMenu2 --> Loop
    ReturnMenu3 --> Loop
    ReturnMenu4 --> Loop
    
    Loop -->|Yes| Input
    Loop -->|No| ExitOp
    
    ExitOp --> End([Terminate<br/>Application])
    
    subgraph "Data Storage Layer"
        Storage[(STORAGE-BALANCE<br/>In-Memory<br/>Initial: 1000.00)]
    end
    
    ReadData -.->|Read| Storage
    ReadCr -.->|Read| Storage
    ReadDb -.->|Read| Storage
    WriteData1 -.->|Write| Storage
    WriteData2 -.->|Write| Storage
    
    style Input fill:#e1f5ff,stroke:#0277bd,stroke-width:2px
    style Decision fill:#fff4e1,stroke:#f57c00,stroke-width:2px
    style ViewOp fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style CreditOp fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style DebitOp fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style ExitOp fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
    style Storage fill:#ffebee,stroke:#c62828,stroke-width:3px
    style ValidateDb fill:#fff9c4,stroke:#f57f17,stroke-width:2px
    style ErrorMsg fill:#ffcdd2,stroke:#d32f2f,stroke-width:2px
```

**Data Transformation Points:**

1. **Input Stage**: User input (1-4) → Menu choice validation
2. **Amount Input**: User amount input → Numeric value (PIC 9(6)V99)
3. **Read Operation**: Storage balance → Working balance
4. **Credit Calculation**: Working balance + Amount → New balance
5. **Debit Validation**: Working balance vs Amount → Boolean decision
6. **Debit Calculation**: Working balance - Amount → New balance
7. **Write Operation**: New balance → Storage balance (persistent)
8. **Display Stage**: Numeric balance → Formatted display string

---

## 5. State Machine Diagram

This diagram shows the application's state transitions based on user actions.

```mermaid
stateDiagram-v2
    [*] --> Initializing: Application Start
    
    Initializing --> DisplayMenu: Initialize Variables<br/>CONTINUE-FLAG = 'YES'<br/>STORAGE-BALANCE = 1000.00
    
    DisplayMenu --> AcceptingInput: Show Menu Options
    
    AcceptingInput --> ViewBalance: User enters 1
    AcceptingInput --> CreditAccount: User enters 2
    AcceptingInput --> DebitAccount: User enters 3
    AcceptingInput --> Exiting: User enters 4
    AcceptingInput --> ErrorHandling: User enters invalid choice
    
    ViewBalance --> ReadingBalance: Call Operations
    ReadingBalance --> DisplayingBalance: Read from DataProgram
    DisplayingBalance --> DisplayMenu: Show balance
    
    CreditAccount --> AcceptingAmount1: Call Operations
    AcceptingAmount1 --> ProcessingCredit: User enters amount
    ProcessingCredit --> ReadingForCredit: Read balance
    ReadingForCredit --> CalculatingCredit: Get current balance
    CalculatingCredit --> WritingCredit: Add amount
    WritingCredit --> DisplayingResult1: Write new balance
    DisplayingResult1 --> DisplayMenu: Show success message
    
    DebitAccount --> AcceptingAmount2: Call Operations
    AcceptingAmount2 --> ProcessingDebit: User enters amount
    ProcessingDebit --> ReadingForDebit: Read balance
    ReadingForDebit --> ValidatingFunds: Get current balance
    
    ValidatingFunds --> CalculatingDebit: Sufficient funds
    ValidatingFunds --> DisplayingError: Insufficient funds
    
    CalculatingDebit --> WritingDebit: Subtract amount
    WritingDebit --> DisplayingResult2: Write new balance
    DisplayingResult2 --> DisplayMenu: Show success message
    
    DisplayingError --> DisplayMenu: Show error message<br/>Balance unchanged
    
    ErrorHandling --> DisplayMenu: Show error message
    
    Exiting --> [*]: STOP RUN<br/>Display "Goodbye"
    
    note right of Initializing
        Initial State:
        - USER-CHOICE = 0
        - CONTINUE-FLAG = 'YES'
        - STORAGE-BALANCE = 1000.00
    end note
    
    note right of ValidatingFunds
        Business Rule:
        IF BALANCE >= AMOUNT
        THEN allow debit
        ELSE reject transaction
    end note
```

**State Descriptions:**

- **Initializing**: Application starts, variables initialized
- **DisplayMenu**: Menu displayed to user, waiting for input
- **AcceptingInput**: Capturing user's menu choice
- **ViewBalance**: Processing view balance request
- **CreditAccount**: Processing credit transaction
- **DebitAccount**: Processing debit transaction
- **ValidatingFunds**: Checking for sufficient funds (business rule)
- **ErrorHandling**: Displaying error for invalid input
- **Exiting**: Terminating application gracefully

---

## 6. Module Dependency Graph

This diagram shows the dependency relationships between program modules and their data structures.

```mermaid
graph TD
    subgraph "External Dependencies"
        USER[User/Console I/O]
    end
    
    subgraph "MainProgram Module"
        MAIN[main.cob<br/>MainProgram]
        MAIN_DATA1[USER-CHOICE<br/>PIC 9]
        MAIN_DATA2[CONTINUE-FLAG<br/>PIC X3]
    end
    
    subgraph "Operations Module"
        OPS[operations.cob<br/>Operations]
        OPS_DATA1[OPERATION-TYPE<br/>PIC X6]
        OPS_DATA2[AMOUNT<br/>PIC 9V6.99]
        OPS_DATA3[FINAL-BALANCE<br/>PIC 9V6.99]
    end
    
    subgraph "DataProgram Module"
        DATA[data.cob<br/>DataProgram]
        DATA_DATA1[STORAGE-BALANCE<br/>PIC 9V6.99]
        DATA_DATA2[OPERATION-TYPE<br/>PIC X6]
    end
    
    USER -->|ACCEPT/DISPLAY| MAIN
    MAIN -->|CALL| OPS
    OPS -->|CALL| DATA
    
    MAIN_DATA1 -.->|Controls| MAIN
    MAIN_DATA2 -.->|Controls| MAIN
    
    OPS_DATA1 -.->|Used by| OPS
    OPS_DATA2 -.->|Used by| OPS
    OPS_DATA3 -.->|Used by| OPS
    
    DATA_DATA1 -.->|Persistent State| DATA
    DATA_DATA2 -.->|Used by| DATA
    
    style MAIN fill:#e1f5ff,stroke:#0277bd,stroke-width:3px
    style OPS fill:#fff4e1,stroke:#f57c00,stroke-width:3px
    style DATA fill:#e8f5e9,stroke:#388e3c,stroke-width:3px
    style USER fill:#f3e5f5,stroke:#7b1fa2,stroke-width:3px
    
    style MAIN_DATA1 fill:#b3e5fc,stroke:#0277bd,stroke-width:1px
    style MAIN_DATA2 fill:#b3e5fc,stroke:#0277bd,stroke-width:1px
    
    style OPS_DATA1 fill:#ffe0b2,stroke:#f57c00,stroke-width:1px
    style OPS_DATA2 fill:#ffe0b2,stroke:#f57c00,stroke-width:1px
    style OPS_DATA3 fill:#ffe0b2,stroke:#f57c00,stroke-width:1px
    
    style DATA_DATA1 fill:#c8e6c9,stroke:#388e3c,stroke-width:1px
    style DATA_DATA2 fill:#c8e6c9,stroke:#388e3c,stroke-width:1px
```

**Dependency Summary:**

| Module | Depends On | Called By | Data Dependencies |
|--------|-----------|-----------|-------------------|
| MainProgram | Operations | None (Entry Point) | USER-CHOICE, CONTINUE-FLAG |
| Operations | DataProgram | MainProgram | OPERATION-TYPE, AMOUNT, FINAL-BALANCE |
| DataProgram | None | Operations | STORAGE-BALANCE, OPERATION-TYPE |

**Call Hierarchy:**
```
User
 └── MainProgram (main.cob)
      └── Operations (operations.cob)
           └── DataProgram (data.cob)
```

**No Circular Dependencies**: The application follows a clean unidirectional call hierarchy with no circular dependencies.

---

## Summary

These diagrams provide comprehensive visualization of:

1. **System Architecture**: Three-tier layered architecture (Presentation, Business Logic, Data Access)
2. **Component Interactions**: How modules and data structures interact
3. **Sequence Flows**: Step-by-step execution paths for all operations
4. **Data Transformations**: How data flows and transforms through the system
5. **State Management**: Application states and transitions
6. **Module Dependencies**: Clear dependency relationships

All diagrams are in Mermaid format and will render properly on GitHub. These visualizations support the modernization effort by clearly documenting the current system's behavior and structure.

---

**Document Version**: 1.0  
**Created**: 2025-11-05  
**Purpose**: Phase 1 - Analysis & Understanding (Issue #3)  
**Next Steps**: Use these diagrams to guide the Node.js conversion (Issue #5)
