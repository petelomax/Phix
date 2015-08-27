dir %1 /-p /o:gn /S > "%temp%\Listing"
start /w notepad "%temp%\Listing"
del "%temp%\Listing"
exit