module Main (main) where

import Data.List
import Debug.Trace

input :: [Int]
input = [1632,1438,1811,1943,1883,1698,1976,1972,1794,1726,1850,1789,1524,1701,1454,1594,1655,1018,1828,1867,1959,1541,1596,1998,1916,1894,1727,1812,1800,1897,1534,1712,1825,1629,1827,81,1855,1621,1694,1663,1793,1685,1616,1899,1688,1652,1719,1589,1649,1742,1905,922,1695,1747,1989,1968,1678,1709,1938,1920,1429,1556,2005,1728,1484,1746,1702,1456,1917,1670,1433,1538,1806,1667,1505,963,1478,2003,1955,1689,1490,1523,1615,1784,1624,583,1465,1443,1489,1873,1485,1773,1704,352,505,1705,1844,1599,1778,1846,1533,1535,1965,1987,828,1755,1823,1639,1981,1763,1758,1819,1569,1580,358,1786,1964,1604,1805,1822,1941,1993,1939,1975,1966,1852,1310,1687,1718,641,1715,1995,1603,1444,1641,1961,1536,1771,1267,1749,1944,1519,1445,1818,1558,1922,1452,1901,1915,1957,1840,1785,1946,1683,1918,1847,1690,1716,1627,1571,1985,1455,435,1856,1527,1660,1555,1557,1591,1906,1646,1656,1620,1618,1598,1606,1808,1509,1551,1723,1835,1610,1820,1942,1767,1549,1607,1781,1612,1864,2007,1908,1650,1449,1886,1878,1895,1869,1469,1507]

main :: IO()
main = do
    let sorted = sort input
    let reversed = reverse sorted
    print ("Sol 1: " ++ (show $ (\(x,y) -> (x,y,x*y)) $ findMatching $ getPairs input))
    print ("Sol 1 improved: " ++ (show $ findMatching' reversed sorted))
    print ("Sol 2: " ++ (show $ (\(x,y,z) -> (x,y,z,x*y*z)) $ findMatching2 $ getTriplets $ getPairs input))

findMatching' :: [Int] -> [Int] -> Int
-- big first - small first
findMatching' [] [] = -1
findMatching' _ [] = -1
findMatching' [] _ = -1
findMatching' (b:bs) (s:ss) | sum == 2020 = b * s
                            | sum > 2020 = findMatching' bs (s:ss)
                            | sum < 2020 = findMatching' (b:bs) ss
                            where sum = b + s

getPairs :: [Int] -> [(Int,Int)]
getPairs [] = []
getPairs [x] = []
getPairs (x:xs) = map (\y -> (x,y)) xs ++ getPairs xs

findMatching :: [(Int,Int)] -> (Int,Int)
findMatching [] = (-1,-1)
findMatching (x:xs) = if fst x + snd x == 2020 then x
                                               else findMatching xs

getTriplets :: [(Int,Int)] -> [(Int,Int,Int)]
getTriplets [] = []
getTriplets (x:xs) = map (\y -> (fst x, snd x, y)) input ++ getTriplets xs

findMatching2 :: [(Int,Int,Int)] -> (Int,Int,Int)
findMatching2 [] = (-1,-1,-1)
findMatching2 (x:xs) = if fst' x + snd' x + trd x == 2020 then x
                                                          else findMatching2 xs

fst' :: (Int,Int,Int) -> Int
fst' (x,_,_) = x

snd' :: (Int,Int,Int) -> Int
snd' (_,x,_) = x

trd :: (Int,Int,Int) -> Int
trd (_,_,x) = x
