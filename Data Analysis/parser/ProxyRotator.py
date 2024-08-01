class ProxyRotator:
    def __init__(self):
        self.proxyList = [
            '198.251.72.26:80',
            '162.251.61.230:3128'
        ]

    def get(self):
        """
        Optionally you could shuffle self.proxyList every X minutes or
        after all proxies had been fetched once ...
        """
        proxy = self.proxyList.pop(0)
        self.proxyList.append(proxy)
        return proxy
