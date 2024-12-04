module X4.XMasSearcher(countAllXmas,countAllCrossMas) where
import Utils.Structures (count)

countXmas' :: Num a => [Char] -> a
countXmas' [] = 0
countXmas' ('S':'A':'M':xs@('X':_)) = 1 + countXmas' xs --pretty smart optimization (could also be solved by reverse of the list into the count method)
countXmas' ('X':'M':'A':xs@('S':_)) = 1 + countXmas' xs
countXmas' (_:xs) = countXmas' xs

countXmas :: Num a => [Char] -> a
countXmas = countXmas'

countAllXmas::Num b => [[Char]] -> b
countAllXmas = sum.map countXmas

type Matrix a = [[a]]

create3x3MatrixScan::Matrix a ->[Matrix a]
create3x3MatrixScan [[r11, r12, r13],[r21, r22, r23],[r31, r32, r33]] = [[[r11, r12, r13], [r21, r22, r23], [r31, r32, r33]]]
create3x3MatrixScan [r11:row1@(r12:r13:_),r21:row2@(r22:r23:_),r31:row3@(r32:r33:_)] = [[r11, r12, r13], [r21, r22, r23], [r31, r32, r33]] : create3x3MatrixScan [row1,row2,row3]
create3x3MatrixScan (r1:rest@(r2:r3:_)) = create3x3MatrixScan [r1,r2,r3] ++ create3x3MatrixScan rest
create3x3MatrixScan _ = []

hasCrossMas :: Matrix Char -> Bool
hasCrossMas [['M', _ ,'M'], [_, 'A', _], ['S', _, 'S']] = True
hasCrossMas [['M', _ ,'S'], [_, 'A', _], ['M', _, 'S']] = True
hasCrossMas [['S', _ ,'M'], [_, 'A', _], ['S', _, 'M']] = True
hasCrossMas [['S', _ ,'S'], [_, 'A', _], ['M', _, 'M']] = True
hasCrossMas _ = False

countCrossMas :: [Matrix Char] -> Int
countCrossMas scan = count True (map hasCrossMas scan)

countAllCrossMas :: Matrix Char -> Int
countAllCrossMas matrix = countCrossMas (create3x3MatrixScan matrix)