### 模型构建与分析
多元线性回归模型调用：lm(formula = Price ~ GSM + Material + Pattern + Opacity, data = reg_data) 
回归结果：
![image](https://github.com/user-attachments/assets/b462e1c5-d887-4f8a-b6e0-c0dc5593d72d)

解释结果：
1. (Intercept) 截距项估计值为43.847715，当GSM、遮光度为0，材质为纯聚酯纤维，模式为简约纹理时，预计价格为43.85美元
2. GSM估计值为0.243801，每增加1 GSM，价格预计平均增加约0.244美元
3. MaterialBlended 估计值为20.174001，使用混纺材质的产品相比纯Polyester，价格平均增加约20.17美元
4. PatternComplex 和Opacity 的显著性水平分别是0.7707，0.9668，均>0.05，对价格的影响不显著
5. 可视化 Pattern 与 Price之间的关系如下
![image (2)](https://github.com/user-attachments/assets/ec71d9d8-9c97-4308-a801-90a70d8e18c0)

6. 可视化 Opacity 与 Price 的关系如下
![image](https://github.com/user-attachments/assets/ee22b52f-1f67-4bce-ab7e-ab7215ed4b16)

### 源代码
```
library(dplyr)`
library(stringr)
library(car)
library(tidyverse)
library(stringr)
library(caret)
library(readxl)

file_path <- "C:\\Users\\PC-HMC\\Desktop\\123.xlsx"
data <- read_excel(file_path)
str(data)          #查看数据结构
sum(is.na(data))   #检查是否有缺失值

# 1. 清洗Material：将纯Polyester标记为"P"，混纺材质标记为"B"
data <- data %>%
  mutate(Material = tolower(Material),
         Material = ifelse(str_detect(Material, "polyester") & !str_detect(Material, "/|\\+|,"), "Pure_Polyester", "Blended"))

# 2. 清洗Pattern：将'Solid'标记为"Simple"，其他标记为"Complex"
data <- data %>%
  mutate(Pattern = ifelse(str_detect(Pattern, regex("solid", ignore_case = TRUE)), "Simple", "Complex"))

# 3. 清洗Opacity：提取数值，计算平均值
clean_opacity <- function(opacity) {
  # 提取所有百分比数字
  nums <- as.numeric(str_extract_all(opacity, "\\d+\\.?\\d*")[[1]])
  if(length(nums) == 0){
    return(NA)
  } else {
    return(mean(nums))
  }
}

data$Opacity_Clean <- sapply(data$Opacity, clean_opacity)

# 4. 清洗GSM：提取数值，计算平均值
clean_gsm <- function(gsm) {
  # 提取所有数字
  nums <- as.numeric(str_extract_all(gsm, "\\d+\\.?\\d*")[[1]])
  if(length(nums) == 0){
    return(NA)
  } else {
    return(mean(nums))
  }
}

data$GSM_Clean <- sapply(data$GSM, clean_gsm)

# 5. 处理Price中的范围（例如，Julian和Nathan有范围）
# 对于Price，我们将取平均值
clean_price <- function(price) {
  # 检查是否有范围
  if(grepl("-", price)) {
    nums <- as.numeric(str_extract_all(price, "\\d+\\.?\\d*")[[1]])
    return(mean(nums))
  } else {
    return(as.numeric(price))
  }
}

# 在此数据集中，Price字段大部分是单一值，但有一些行可能包含范围
# 例如，Julian和Nathan的GSM包含范围，需要特别处理
# 但Price似乎没有范围，所以此步骤可能不需要
# 但为了稳妥，保留处理步骤

data$Price_Clean <- as.numeric(data$Price)  # 当前数据中Price无范围

# 6. 处理GSM_Clean中的范围
# 已在GSM_Clean列中完成

# 查看清洗后的数据
head(data)


# 选择需要的列
reg_data <- data %>%
  select(Price = Price_Clean, GSM = GSM_Clean, Material, Pattern, Opacity = Opacity_Clean) %>%
  na.omit()  # 移除缺失值

# 转换为因子变量
reg_data$Material <- factor(reg_data$Material, levels = c("Pure_Polyester", "Blended"))
reg_data$Pattern <- factor(reg_data$Pattern, levels = c("Simple", "Complex"))

# 建立回归模型
model <- lm(Price ~ GSM + Material + Pattern + Opacity, data = reg_data)

# 查看回归结果
summary(model)


# 可视化Pattern与Price的关系
library(ggplot2)

ggplot(reg_data, aes(x = Pattern, y = Price)) +
  geom_boxplot() +
  labs(title = "Price vs Pattern", x = "Pattern", y = "Price")

# 可视化Opacity与Price的关系
ggplot(reg_data, aes(x = Opacity, y = Price)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Price vs Opacity", x = "Opacity (%)", y = "Price")

# 查看异常值
outliers <- reg_data %>% filter(abs(std_resid) > 3)
print(outliers)

# 计算影响度量（Cook's distance）
cooksd <- cooks.distance(model)

# 识别高影响点
high_influence <- reg_data %>% filter(cooksd > (4/(nrow(reg_data) - length(model$coefficients) - 1)))
print(high_influence)
```
[123.xlsx](https://github.com/user-attachments/files/18244570/123.xlsx)

