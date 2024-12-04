-- Populating the nulls in Property Address Column
-- Finding the number of nulls in Property Address column
select COUNT(*)
from Nashville_Housing
where PropertyAddress is null 

-- Since the PacrselID is in one-to-one relationship with the Property Address, So we will populate the nulls in Property Address column that has a specific PacrselID with the Property Address that has the same PacrselID 
update tab1
set PropertyAddress = ISNULL(tab1.PropertyAddress, tab2.PropertyAddress)
from Nashville_Housing as tab1
join Nashville_Housing as tab2
on tab1.ParcelID = tab2.ParcelID And tab1.UniqueID != tab2.UniqueID
where tab1.PropertyAddress is null


-- Breaking out Property Address column into (Address, State)
-- Adding a new column for the poperty split address
ALTER table Nashville_Housing
ADD PropertySplitAddress nvarchar(225);
--Populating the PropertySplitAddress column
Update Nashville_Housing
set PropertySplitAddress = LEFT(PropertyAddress, (charindex(',', PropertyAddress) - 1))

-- Adding a new column for the poperty split city
ALTER table Nashville_Housing
ADD PropertySplitCity nvarchar(225);
--Populating the PropertySplitCity column
Update Nashville_Housing
set PropertySplitCity = Right(PropertyAddress, (LEN(PropertyAddress) - charindex(',', PropertyAddress) - 1))


-- Breaking out Owner Address column into (Address, City, State)
-- Adding a new column for the Owner Split Address
ALTER table Nashville_Housing
ADD OwnerSplitAddress nvarchar(225);
--Populating the OwnerSplitAddress column
Update Nashville_Housing
set OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.'),3)

-- Adding a new column for the Owner Split City
ALTER table Nashville_Housing
ADD OwnerSplitCity nvarchar(225);
--Populating the OwnerSplitCity column
Update Nashville_Housing
set OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',', '.'),2)

-- Adding a new column for the Owner Split State
ALTER table Nashville_Housing
ADD OwnerSplitState nvarchar(225);
--Populating the OwnerSplitState column
Update Nashville_Housing
set OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',', '.'),1)


--Deleting unused columns
ALTER TABLE Nashville_Housing  
Drop COLUMN PropertyAddress, OwnerAddress


-- Removing duplicated rows
with RowNumberCTE as (
select *,
       ROW_NUMBER() OVER (PARTITION  by ParcelID,
								      PropertyAddress,
									  SalePrice,
									  SaleDate,
									  LegalReference
                         Order by UniqueID) RowNumber
from Nashville_Housing)
Delete
from RowNumberCTE
where RowNumber > 1
