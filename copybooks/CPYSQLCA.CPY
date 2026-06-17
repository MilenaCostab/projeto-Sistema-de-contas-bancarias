       01 SQLCA.
           05 SQLCAID          PIC X(08) VALUE 'SQLCA'.
           05 SQLCABC          PIC S9(09) COMP VALUE 136.
           05 SQLCODE          PIC S9(09) COMP VALUE 0.
           05 SQLERRM.
               10 SQLERRML     PIC S9(04) COMP VALUE 0.
               10 SQLERRMC     PIC X(70)  VALUE SPACES.
           05 SQLERRP          PIC X(08) VALUE SPACES.
           05 SQLERRD OCCURS 6 TIMES
                               PIC S9(09) COMP VALUE 0.
           05 SQLWARN.
               10 SQLWARN0     PIC X(01) VALUE SPACES.
               10 SQLWARN1     PIC X(01) VALUE SPACES.
               10 SQLWARN2     PIC X(01) VALUE SPACES.
               10 SQLWARN3     PIC X(01) VALUE SPACES.
               10 SQLWARN4     PIC X(01) VALUE SPACES.
               10 SQLWARN5     PIC X(01) VALUE SPACES.
               10 SQLWARN6     PIC X(01) VALUE SPACES.
               10 SQLWARN7     PIC X(01) VALUE SPACES.
           05 SQLSTATE         PIC X(05) VALUE SPACES.