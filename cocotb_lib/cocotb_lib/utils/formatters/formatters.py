
def as_mac_address(address: int) -> str:
    hex_str = f'{address:012x}'
    mac_address = ':'.join(hex_str[i:i+2] for i in range(0, 12, 2))
    return mac_address

def as_ip_address(address: int):                                                                                                                                                                                    
    # Ensure the integer is within the valid range for IPv4 addresses
    if not (0 <= address <= 0xFFFFFFFF):
        raise ValueError("Integer is out of the valid range for IPv4 addresses")
                                                                                                                                                                                                            
    # Convert the integer to an IP address                                                                                                                                                                         
    ip_address = '.'.join(str((address >> (8 * i)) & 0xFF) for i in reversed(range(4))) 
    return ip_address
