module X4.Depcrecated() where
import Utils.Structures (count)

type Matrix a = [[a]]
data Row3 a = Row3 a a a deriving Show
data Matrix3 a = Matrix3 (Row3 a) (Row3 a) (Row3 a) deriving Show

create3x3MatrixScan'::Matrix a -> Matrix a->[Matrix3 a]
create3x3MatrixScan' [_,_,_] [[r11, r12, r13],[r21, r22, r23],[r31, r32, r33]] = [Matrix3 (Row3 r11 r12 r13) (Row3 r21 r22 r23) (Row3 r31 r32 r33)]
create3x3MatrixScan' (_:orig) ([r11, r12, r13]:[r21, r22, r23]:[r31, r32, r33]:_) = Matrix3 (Row3 r11 r12 r13) (Row3 r21 r22 r23) (Row3 r31 r32 r33) : create3x3MatrixScan' orig orig
create3x3MatrixScan' orig ((r11:row1@(r12:r13:_)):(r21:row2@(r22:r23:_)):(r31:row3@(r32:r33:_)):rs) = Matrix3 (Row3 r11 r12 r13) (Row3 r21 r22 r23) (Row3 r31 r32 r33) : create3x3MatrixScan' orig (row1:row2:row3:rs)
create3x3MatrixScan' _ _ = []

create3x3MatrixScan''::Matrix a -> Matrix a->[Matrix a]
create3x3MatrixScan'' [_,_,_] [[r11, r12, r13],[r21, r22, r23],[r31, r32, r33]] = [[[r11, r12, r13], [r21, r22, r23], [r31, r32, r33]]]
create3x3MatrixScan'' (_:orig) ([r11, r12, r13]:[r21, r22, r23]:[r31, r32, r33]:_) = [[r11, r12, r13], [r21, r22, r23], [r31, r32, r33]] : create3x3MatrixScan'' orig orig
create3x3MatrixScan'' orig ((r11:row1@(r12:r13:_)):(r21:row2@(r22:r23:_)):(r31:row3@(r32:r33:_)):rs) = [[r11, r12, r13], [r21, r22, r23], [r31, r32, r33]] : create3x3MatrixScan'' orig (row1:row2:row3:rs)
create3x3MatrixScan'' _ _ = []

create3x3MatrixScan :: Matrix a -> [Matrix3 a]
create3x3MatrixScan xs = create3x3MatrixScan' xs xs

showMatrix3 :: Matrix3 a -> [[a]]
showMatrix3 (Matrix3 (Row3 r1 r2 r3) (Row3 r4 r5 r6) (Row3 r7 r8 r9)) = [[r1,r2,r3],[r4,r5,r6],[r7,r8,r9]]

hasCrossMas :: Matrix3 Char -> Bool
hasCrossMas (Matrix3 (Row3 'M' _ 'M') (Row3 _ 'A' _) (Row3 'S' _ 'S')) = True
hasCrossMas (Matrix3 (Row3 'M' _ 'S') (Row3 _ 'A' _) (Row3 'M' _ 'S')) = True
hasCrossMas (Matrix3 (Row3 'S' _ 'M') (Row3 _ 'A' _) (Row3 'S' _ 'M')) = True
hasCrossMas (Matrix3 (Row3 'S' _ 'S') (Row3 _ 'A' _) (Row3 'M' _ 'M')) = True
hasCrossMas _ = False 

countCrossMas :: [Matrix3 Char] -> Int
countCrossMas scan = count True (map hasCrossMas scan)

countAllCrossMas :: Matrix Char -> Int
countAllCrossMas matrix = countCrossMas (create3x3MatrixScan matrix)