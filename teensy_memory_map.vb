Imports System.IO

''' <summary>
''' ----------------------------------------------------------------------------------------------------------------------------------------------
''' Author        : Joe Churchwell
''' Date          : 2018-08-15
''' Description   : Created to produce a memory map for the Teensy ARM boards and display the information in the Arduino IDE.
''' How to use    : This assumes you have Arduino and the Teensy Arduino Add-In
'''                 1) Compile the VB.NET source to the release directory
'''                 2) Navigate to the Arduino hardware tools directory
'''                 3) Re-name the file 'teensy_post_compile.exe' to 'REAL_teensy_post_compile.exe'
'''                 4) Copy the file 'teensy_post_compile.exe' from the release directory and place it in the Arduino-hardware-tools directory
'''                       i.e. 'C:\Program Files (x86)\Arduino\hardware\tools\'
'''                 5) Verify, through the Arduino IDE preferences (File->Preferences), that the 'Show verbose output during complie' is checked
'''                 6) Test the memory map generation Using the Arduino IDE and note that it displays the memory map in the compile output window
''' -----------------------------------------------------------------------------------------------------------------------------------------------
''' </summary>
''' 
Module teensy_memory_map
    ''' <summary>
    '''     This utility is used to create the map file from from the *.elf output file
    ''' </summary>
    ''' <param name="cmdArgs">The command line arguments aren't currently used</param>
    Sub Main(ByVal cmdArgs() As String)

        Dim output_path As String = ""
        Dim output_file As String = ""

        Dim buildArgString As String = "" ' This is used so we can relay the output to the real teensy post compile utility
        Dim shell_output As String = ""

        Dim fileRead As String
        Dim arrLines() As String
        Dim strFields() As String

        Dim tmpBytes As UInt32

        Dim listOut As New List(Of String)

        ' Constants

        ' ARGUMENT SWITCH CONSTRUCT IDENTIFIERS
        ' [ex. "-file=PROJECT_NAME.ino" "-path=C:\Users\USER_NAME\AppData\Local\Temp\arduino_build_#####" "-tools=C:\Program Files (x86)\Arduino\hardware\teensy/../tools/" "-board=TEENSY31"]
        Const PATH_ID As String = "-path=" ' file path argument switch
        Const FILE_ID As String = "-file=" ' file argument switch

        ' The real teensy_post_compile utility. The origional one will have to be re-named to something different. I chose tp re-name it "REAL_teensy_post_compile.exe"
        Const TEENSY_POST_COMPILER As String = "REAL_teensy_post_compile.exe" ' "C:\Program Files (x86)\Arduino\hardware\tools\REAL_teensy_post_compile.exe"

        Const ADDRESS_FIELD_NUM As UInt16 = 0
        Const GLOBAL_G_FIELD_NUM As UInt16 = 1, GLOBAL_G_FIELD_DESIGNATOR As String = "g"
        Const OBJECT_O_FIELD_NUM As UInt16 = 2, OBJECT_O_FIELD_DESIGNATOR As String = "O"
        Const OBJECT_BYTE_COUNT_NUM As UInt16 = 4
        Const OBJECT_NAME_NUM As UInt16 = 5
        Const OUTPUT_TAB_SPACING As UInt16 = 64
        Const NUMBER_OF_FIELDS As UInt16 = OBJECT_NAME_NUM ' Set this to the last element number

        ' Loop through all the command line arguments
        For Each iStr In cmdArgs

            ' Check to see if it is an argument we need to be able to process the map file
            If iStr.Contains(PATH_ID) Then
                output_path = Replace(iStr, PATH_ID, "")
            ElseIf iStr.Contains(FILE_ID) Then
                output_file = Replace(iStr, FILE_ID, "")
            End If

            ' We also need to build a string for the "REAL_teensy_post_compile.exe" application
            buildArgString += """" & iStr & """" & " "

        Next

        ' Check to see we have found a file and path for the *.elf file so we can continue to process
        If output_file <> "" And output_path <> "" Then

            ' Run avr-objdump with '-t' followed by elf file name to generate the map file
            shell_output = "cmd /c avr-objdump.exe -t " & output_path & "\" & output_file & ".elf" & " > " & output_path & "\" & output_file & ".map"
            Shell(shell_output, vbMaximizedFocus, True)

            ' Read in all the data that was output by avr-objdump.exe
            fileRead = My.Computer.FileSystem.ReadAllText(output_path & "\" & output_file & ".map")

            ' Split the file contents into an array of lines to process wach line
            arrLines = Split(fileRead, vbCrLf)

            ' Loop through all the lines and expose specific fields
            For Each iLine In arrLines

                ' First replace any tabs with spaces to create a common delimiter
                iLine = Replace(iLine, vbTab, " ")

                ' Separate the line into specific fields
                strFields = SplitMultiDelims(iLine, " ", True)

                ' Make sure we can process the fields
                If UBound(strFields) > NUMBER_OF_FIELDS - 1 Then
                    ' Look for variables that were created in our application
                    If strFields(GLOBAL_G_FIELD_NUM) = GLOBAL_G_FIELD_DESIGNATOR AndAlso strFields(OBJECT_O_FIELD_NUM) = OBJECT_O_FIELD_DESIGNATOR Then
                        ' Output the information we care about to the Arduino output window
                        tmpBytes = Convert.ToUInt32(strFields(OBJECT_BYTE_COUNT_NUM), 16)
                        listOut.Add(strFields(OBJECT_NAME_NUM) & StrDup(OUTPUT_TAB_SPACING - strFields(OBJECT_NAME_NUM).Length, " ") & Mid(strFields(ADDRESS_FIELD_NUM), 1, 4) & ":" & Mid(strFields(ADDRESS_FIELD_NUM), 5) & StrDup(16 - tmpBytes.ToString.Length, " ") & tmpBytes)
                        'Console.Write(strFields(OBJECT_NAME_NUM) & StrDup(OUTPUT_TAB_SPACING - strFields(OBJECT_NAME_NUM).Length, " ") & Mid(strFields(ADDRESS_FIELD_NUM), 1, 4) & ":" & Mid(strFields(ADDRESS_FIELD_NUM), 5) & StrDup(16 - tmpBytes.ToString.Length, " ") & tmpBytes & vbLf)
                    End If
                End If

            Next

            listOut.Sort()

            For Each istr In listOut
                Console.Write(istr & vbLf)
            Next

        End If

        ' Check if there is a hex file and display the CRC32 value
        Console.Write("***************** CRC CALCULATIONS *******************" & vbLf)
        If My.Computer.FileSystem.FileExists(output_path & "\" & output_file & ".elf") Then
            Console.Write(vbTab & "ELF CRC = " & GetCRC32(output_path & "\" & output_file & ".elf") & vbLf)
        End If
        If My.Computer.FileSystem.FileExists(output_path & "\" & output_file & ".hex") Then
            Console.Write(vbTab & "HEX CRC = " & GetCRC32(output_path & "\" & output_file & ".hex") & vbLf)
        End If
        If My.Computer.FileSystem.FileExists(output_path & "\" & output_file & ".map") Then
            Console.Write(vbTab & "MAP CRC = " & GetCRC32(output_path & "\" & output_file & ".map") & vbLf)
        End If
        Console.Write("**************** END CRC CALCULATIONS ****************" & vbLf)

        ' Make sure to run the real teensy post compile utility
        Shell(TEENSY_POST_COMPILER & " " & buildArgString)

    End Sub

    ''' <summary>
    '''     SplitMultiDelims by alainbryden
    '''     This function splits Text into an array of substrings, each substring
    '''     delimited by any character in DelimChars. Only a single character
    '''     may be a delimiter between two substrings, but DelimChars may
    '''     contain any number of delimiter characters. It returns a single element
    '''     array containing all of text if DelimChars is empty, or a 1 or greater
    '''     element array if the Text is successfully split into substrings.
    '''     If IgnoreConsecutiveDelimiters is true, empty array elements will not occur.
    '''     If Limit greater than 0, the function will only split Text into 'Limit'
    '''     array elements or less. The last element will contain the rest of Text.
    '''     https://www.experts-exchange.com/articles/1480/How-to-Split-a-String-with-Multiple-Delimiters-in-VBA.html
    ''' </summary>
    ''' <param name="Text">Text to be separated</param>
    ''' <param name="DelimChars">Char to use as the delimeter</param>
    ''' <param name="IgnoreConsecutiveDelimiters">Optional - Boolean to determine if consecutive delimiters should be overlooked</param>
    ''' <param name="Limit">Optional - Number of elements to limit to</param>
    ''' <returns>The split char array</returns>
    Function SplitMultiDelims(ByRef Text As String, ByRef DelimChars As String,
            Optional ByVal IgnoreConsecutiveDelimiters As Boolean = False,
            Optional ByVal Limit As Long = -1) As String()
        Dim ElemStart As Long, N As Long, M As Long, Elements As Long
        Dim lDelims As Long, lText As Long
        Dim Arr() As String

        lText = Len(Text)
        lDelims = Len(DelimChars)
        If lDelims = 0 Or lText = 0 Or Limit = 1 Then
            ReDim Arr(0 To 0)
            Arr(0) = Text
            SplitMultiDelims = Arr
            Exit Function
        End If
        ReDim Arr(0 To IIf(Limit = -1, lText - 1, Limit))

        Elements = 0 : ElemStart = 1
        For N = 1 To lText
            If InStr(DelimChars, Mid(Text, N, 1)) Then
                Arr(Elements) = Mid(Text, ElemStart, N - ElemStart)
                If IgnoreConsecutiveDelimiters Then
                    If Len(Arr(Elements)) > 0 Then Elements = Elements + 1
                Else
                    Elements = Elements + 1
                End If
                ElemStart = N + 1
                If Elements + 1 = Limit Then Exit For
            End If
        Next N
        'Get the last token terminated by the end of the string into the array
        If ElemStart <= lText Then Arr(Elements) = Mid(Text, ElemStart)
        'Since the end of string counts as the terminating delimiter, if the last character
        'was also a delimiter, we treat the two as consecutive, and so ignore the last elemnent
        If IgnoreConsecutiveDelimiters Then If Len(Arr(Elements)) = 0 Then Elements = Elements - 1

        ReDim Preserve Arr(0 To Elements) 'Chop off unused array elements
        SplitMultiDelims = Arr
    End Function

    ''' <summary>
    '''     This function is used to determine the CRC of a binary file
    ''' </summary>
    ''' <param name="sFileName">The file name and path to process a CRC for</param>
    ''' <returns>The calcualted CRC based on 'sFileName'</returns>
    Public Function GetCRC32(ByVal sFileName As String) As String
        Try
            Dim FS As FileStream = New FileStream(sFileName, FileMode.Open, FileAccess.Read, FileShare.Read, 8192)
            Dim CRC32Result As Integer = &HFFFFFFFF
            Dim Buffer(4096) As Byte
            Dim ReadSize As Integer = 4096
            Dim Count As Integer = FS.Read(Buffer, 0, ReadSize)
            Dim CRC32Table(256) As Integer
            Dim DWPolynomial As Integer = &HEDB88320
            Dim DWCRC As Integer
            Dim i As Integer, j As Integer, n As Integer
            'Create CRC32 Table
            For i = 0 To 255
                DWCRC = i
                For j = 8 To 1 Step -1
                    If (DWCRC And 1) Then
                        DWCRC = ((DWCRC And &HFFFFFFFE) \ 2&) And &H7FFFFFFF
                        DWCRC = DWCRC Xor DWPolynomial
                    Else
                        DWCRC = ((DWCRC And &HFFFFFFFE) \ 2&) And &H7FFFFFFF
                    End If
                Next j
                CRC32Table(i) = DWCRC
            Next i
            'Calcualting CRC32 Hash
            Do While (Count > 0)
                For i = 0 To Count - 1
                    n = (CRC32Result And &HFF) Xor Buffer(i)
                    CRC32Result = ((CRC32Result And &HFFFFFF00) \ &H100) And &HFFFFFF
                    CRC32Result = CRC32Result Xor CRC32Table(n)
                Next i
                Count = FS.Read(Buffer, 0, ReadSize)
            Loop
            Return Hex(Not (CRC32Result))
        Catch ex As Exception
            Return ""
        End Try
    End Function

End Module

